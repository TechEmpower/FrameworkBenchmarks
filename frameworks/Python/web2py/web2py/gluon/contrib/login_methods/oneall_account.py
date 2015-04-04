#!/usr/bin/env python
# coding: utf8

"""
   Oneall Authentication for web2py
   Developed by Nathan Freeze (Copyright © 2013)
   Email <nathan@freezable.com>

   This file contains code to allow using onall.com
   authentication services with web2py
"""

import os
import base64
from gluon import *
from gluon.storage import Storage
from gluon.contrib.simplejson import JSONDecodeError
from gluon.tools import fetch
import gluon.contrib.simplejson as json

class OneallAccount(object):

    """
    from gluon.contrib.login_methods.oneall_account import OneallAccount
    auth.settings.actions_disabled=['register','change_password',
        'request_reset_password']
    auth.settings.login_form = OneallAccount(request,
              public_key="...",
              private_key="...",
              domain="...",
              url = "http://localhost:8000/%s/default/user/login" % request.application)
    """

    def __init__(self, request, public_key="", private_key="",  domain="",
                       url=None, providers=None, on_login_failure=None):

        self.request = request
        self.public_key = public_key
        self.private_key = private_key
        self.url = url
        self.domain = domain
        self.profile = None
        self.on_login_failure = on_login_failure
        self.providers = providers or ["facebook", "google", "yahoo", "openid"]

        self.mappings = Storage()
        def defaultmapping(profile):
            name = profile.get('name',{})
            dname = name.get('formatted',profile.get('displayName'))
            email=profile.get('emails', [{}])[0].get('value')
            reg_id=profile.get('identity_token','')
            username=profile.get('preferredUsername',email)
            first_name=name.get('givenName', dname.split(' ')[0])
            last_name=profile.get('familyName',dname.split(' ')[1])
            return dict(registration_id=reg_id,username=username,email=email,
                        first_name=first_name,last_name=last_name)
        self.mappings.default = defaultmapping

    def get_user(self):
        request = self.request
        user = None
        if request.vars.connection_token:
            auth_url = "https://%s.api.oneall.com/connections/%s.json"  % \
                       (self.domain, request.vars.connection_token)
            auth_pw = "%s:%s" % (self.public_key,self.private_key)
            auth_pw = base64.b64encode(auth_pw)
            headers = dict(Authorization="Basic %s" % auth_pw)
            try:
                auth_info_json = fetch(auth_url,headers=headers)
                auth_info = json.loads(auth_info_json)
                data = auth_info['response']['result']['data']
                if data['plugin']['key'] == 'social_login':
                    if data['plugin']['data']['status'] == 'success':
                        userdata = data['user']
                        self.profile = userdata['identity']
                        source = self.profile['source']['key']
                        mapping = self.mappings.get(source,self.mappings['default'])
                        user = mapping(self.profile)
            except (JSONDecodeError, KeyError):
                pass
            if user is None and self.on_login_failure:
                    redirect(self.on_login_failure)
        return user

    def login_form(self):
        scheme = self.request.env.wsgi_url_scheme
        oneall_url = scheme + "://%s.api.oneall.com/socialize/library.js"  % self.domain
        oneall_lib = SCRIPT(_src=oneall_url,_type='text/javascript')
        container = DIV(_id="oa_social_login_container")
        widget = SCRIPT('oneall.api.plugins.social_login.build("oa_social_login_container",',
                        '{providers : %s,' % self.providers,
                        'callback_uri: "%s"});' % self.url,
                _type="text/javascript")
        form = DIV(oneall_lib,container,widget)
        return form

def use_oneall(auth, filename='private/oneall.key', **kwargs):
    path = os.path.join(current.request.folder, filename)
    if os.path.exists(path):
        request = current.request
        domain, public_key, private_key = open(path, 'r').read().strip().split(':')
        url = URL('default', 'user', args='login', scheme=True)
        auth.settings.actions_disabled =\
        ['register', 'change_password', 'request_reset_password']
        auth.settings.login_form = OneallAccount(
            request, public_key=public_key,private_key=private_key,
            domain=domain, url=url, **kwargs)
