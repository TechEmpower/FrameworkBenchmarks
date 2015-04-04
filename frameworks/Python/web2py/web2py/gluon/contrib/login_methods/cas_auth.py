#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of web2py Web Framework (Copyrighted, 2007-2009).
Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu>.
License: LGPL v3

Tinkered by Szabolcs Gyuris < szimszo n @ o regpreshaz dot eu>
"""

from gluon import current, redirect, URL


class CasAuth(object):
    """
    Login will be done via Web2py's CAS application, instead of web2py's
    login form.

    Include in your model (eg db.py)::

        from gluon.contrib.login_methods.cas_auth import CasAuth
        auth.define_tables(username=True)
        auth.settings.login_form=CasAuth(
            urlbase = "https://[your CAS provider]/app/default/user/cas",
                       actions=['login','validate','logout'])

    where urlbase is the actual CAS server url without the login,logout...
    Enjoy.

    ###UPDATE###
    if you want to connect to a CAS version 2 JASIG Server use this:
        auth.settings.login_form=CasAuth(
            urlbase = "https://[Your CAS server]/cas",
            actions = ['login','serviceValidate','logout'],
            casversion = 2,
            casusername = "cas:user")

    where casusername is the xml node returned by CAS server which contains
    user's username.

    """
    def __init__(self, g=None,  # g for backward compatibility ###
                 urlbase="https://web2py.com/cas/cas",
                 actions=['login', 'validate', 'logout'],
                 maps=dict(username=lambda v: v.get('username', v['user']),
                           email=lambda v: v.get('email', None),
                           user_id=lambda v: v['user']),
                 casversion=1,
                 casusername='cas:user'
                 ):
        self.urlbase = urlbase
        self.cas_login_url = "%s/%s" % (self.urlbase, actions[0])
        self.cas_check_url = "%s/%s" % (self.urlbase, actions[1])
        self.cas_logout_url = "%s/%s" % (self.urlbase, actions[2])
        self.maps = maps
        self.casversion = casversion
        self.casusername = casusername
        # vars commented because of
        # https://code.google.com/p/web2py/issues/detail?id=1774
        self.cas_my_url = URL(args=current.request.args,
                              #vars=current.request.vars, 
                              scheme=True)

    def login_url(self, next="/"):
        current.session.token = self._CAS_login()
        return next

    def logout_url(self, next="/"):
        current.session.token = None
        current.session.auth = None
        self._CAS_logout()
        return next

    def get_user(self):
        user = current.session.token
        if user:
            d = {'source': 'web2py cas'}
            for key in self.maps:
                d[key] = self.maps[key](user)
            return d
        return None

    def _CAS_login(self):
        """
        exposed as CAS.login(request)
        returns a token on success, None on failed authentication
        """
        import urllib
        self.ticket = current.request.vars.ticket
        if not current.request.vars.ticket:
            redirect("%s?service=%s" % (self.cas_login_url,
                                        self.cas_my_url))
        else:
            url = "%s?service=%s&ticket=%s" % (self.cas_check_url,
                                               self.cas_my_url,
                                               self.ticket)
            data = urllib.urlopen(url).read()
            if data.startswith('yes') or data.startswith('no'):
                data = data.split('\n')
                if data[0] == 'yes':
                    if ':' in data[1]:  # for Compatibility with Custom CAS
                        items = data[1].split(':')
                        a = items[0]
                        b = len(items) > 1 and items[1] or a
                        c = len(items) > 2 and items[2] or b
                    else:
                        a = b = c = data[1]
                    return dict(user=a, email=b, username=c)
                return None
            import xml.dom.minidom as dom
            import xml.parsers.expat as expat
            try:
                dxml = dom.parseString(data)
                envelop = dxml.getElementsByTagName(
                    "cas:authenticationSuccess")
                if len(envelop) > 0:
                    res = dict()
                    for x in envelop[0].childNodes:
                        if x.nodeName.startswith('cas:') and len(x.childNodes):
                            key = x.nodeName[4:].encode('utf8')
                            value = x.childNodes[0].nodeValue.encode('utf8')
                            if not key in res:
                                res[key] = value
                            else:
                                if not isinstance(res[key], list):
                                    res[key] = [res[key]]
                                res[key].append(value)
                    return res
            except expat.ExpatError:
                pass
            return None  # fallback

    def _CAS_logout(self):
        """
        exposed CAS.logout()
        redirects to the CAS logout page
        """
        import urllib
        redirect("%s?service=%s" % (self.cas_logout_url, self.cas_my_url))
