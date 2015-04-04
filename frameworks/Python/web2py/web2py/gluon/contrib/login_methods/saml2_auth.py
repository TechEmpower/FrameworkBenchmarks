#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of web2py Web Framework (Copyrighted, 2007-2014).
Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu>.
License: LGPL v3

Login will be done via Web2py's CAS application, instead of web2py's
login form.

Include in your model (eg db.py)::

    auth.define_tables(username=True)
    from gluon.contrib.login_methods.saml2_auth import Saml2Auth
    auth.settings.login_form=Saml2Auth(
    config_file = os.path.join(request.folder,'private','sp_conf'),
    maps=dict(
        username=lambda v: v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0],
        email=lambda v: v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0],
        user_id=lambda v: v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0]))
        
you must have private/sp_conf.py, the pysaml2 sp configuration file
"""

from saml2 import BINDING_HTTP_REDIRECT
from saml2.client import Saml2Client
from gluon.utils import web2py_uuid
from gluon import current, redirect, URL
import os, types

def obj2dict(obj, processed=None):
    """                                                                        
    converts any object into a dict, recursively                               
    """
    processed = processed if not processed is None else set()
    if obj is None:
        return None
    if isinstance(obj,(int,long,str,unicode,float,bool)):
        return obj
    if id(obj) in processed:
        return '<reference>'
    processed.add(id(obj))
    if isinstance(obj,(list,tuple)):
        return [obj2dict(item,processed) for item in obj]
    if not isinstance(obj, dict) and hasattr(obj,'__dict__'):
        obj = obj.__dict__
    else:
        return repr(obj)
    return dict((key,obj2dict(value,processed)) for key,value in obj.items()
                if not key.startswith('_') and
                not type(value) in (types.FunctionType,
                                    types.LambdaType,
                                    types.BuiltinFunctionType,
                                    types.BuiltinMethodType))

def saml2_handler(session, request, config_filename = None):
    config_filename = config_filename or os.path.join(request.folder,'private','sp_conf')
    client = Saml2Client(config_file = config_filename)
    idps = client.metadata.with_descriptor("idpsso")
    entityid = idps.keys()[0]
    bindings = [BINDING_HTTP_REDIRECT]
    binding, destination = client.pick_binding(
        "single_sign_on_service", bindings, "idpsso", entity_id=entityid)
    binding = BINDING_HTTP_REDIRECT 
    if not request.vars.SAMLResponse:
        req_id, req = client.create_authn_request(destination, binding=binding)
        relay_state = web2py_uuid().replace('-','')
        session.saml_outstanding_queries = {req_id: request.url}
        session.saml_req_id = req_id
        http_args = client.apply_binding(binding, str(req), destination,
                                         relay_state=relay_state)
        return {'url':dict(http_args["headers"])['Location']}
    else:
        relay_state = request.vars.RelayState
        req_id = session.saml_req_id
        unquoted_response = request.vars.SAMLResponse
        res =  {}
        try:
            data = client.parse_authn_request_response(
                unquoted_response, binding, session.saml_outstanding_queries)
            res['response'] = data if data else {}
        except Exception, e:
            import traceback
            res['error'] = traceback.format_exc()
        return res
    

class Saml2Auth(object):

    def __init__(self, config_file=None, maps=dict(
            username=lambda v:v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0],
            email=lambda v:v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0],
            user_id=lambda v:v['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/upn'][0],
            )):
        self.config_file = config_file
        self.maps = maps

    def login_url(self, next="/"):
        d = saml2_handler(current.session, current.request)
        if 'url' in d:
            redirect(d['url'])
        elif 'error' in d:
            current.session.flash = d['error']
            redirect(URL('default','index'))
        elif 'response' in d:            
            # a['assertions'][0]['attribute_statement'][0]['attribute']
            # is list of
            # {'name': 'http://schemas.microsoft.com/ws/2008/06/identity/claims/windowsaccountname', 'name_format': None, 'text': None, 'friendly_name': None, 'attribute_value': [{'text': 'CAA\\dev-mdp', 'extension_attributes': "{'{http://www.w3.org/2001/XMLSchema-instance}type': 'xs:string'}", 'extension_elements': []}], 'extension_elements': [], 'extension_attributes': '{}'}
            try:
                attributes = d['response'].assertions[0].attribute_statement[0].attribute
            except:
                attributes = d['response'].assertion.attribute_statement[0].attribute
            current.session.saml2_info = dict(
                (a.name, [i.text for i in a.attribute_value]) for a in attributes)
        return next

    def logout_url(self, next="/"):
        current.session.saml2_info = None
        return next

    def get_user(self):        
        user = current.session.saml2_info
        if user:
            d = {'source': 'web2py saml2'}
            for key in self.maps:
                d[key] = self.maps[key](user)
            return d
        return None
