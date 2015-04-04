#!/usr/bin/env python
# coding: utf8

"""
   LoginRadius Authentication for web2py
   Developed by Nathan Freeze (Copyright © 2013)
   Email <nathan@freezable.com>

   This file contains code to allow using loginradius.com
   authentication services with web2py
"""

import os
from gluon import *
from gluon.storage import Storage
from gluon.contrib.simplejson import JSONDecodeError
from gluon.tools import fetch
import gluon.contrib.simplejson as json


class LoginRadiusAccount(object):
    """
    from gluon.contrib.login_methods.loginradius_account import LoginRadiusAccount
    auth.settings.actions_disabled=['register','change_password',
        'request_reset_password']
    auth.settings.login_form = LoginRadiusAccount(request,
              api_key="...",
              api_secret="...",
              url = "http://localhost:8000/%s/default/user/login" % request.application)
    """

    def __init__(self, request, api_key="", api_secret="",
                 url="", on_login_failure=None):

        self.request = request
        self.api_key = api_key
        self.api_secret = api_secret
        self.url = url
        self.auth_base_url = "https://hub.loginradius.com/UserProfile.ashx/"
        self.profile = None
        self.on_login_failure = on_login_failure
        self.mappings = Storage()

        def defaultmapping(profile):
            first_name = profile.get('FirstName')
            last_name = profile.get('LastName')
            email = profile.get('Email', [{}])[0].get('Value')
            reg_id = profile.get('ID', '')
            username = profile.get('ProfileName', email)

            return dict(registration_id=reg_id, username=username, email=email,
                        first_name=first_name, last_name=last_name)

        self.mappings.default = defaultmapping

    def get_user(self):
        request = self.request
        user = None
        if request.vars.token:
            try:
                auth_url = self.auth_base_url + self.api_secret + "/" + request.vars.token
                json_data = fetch(auth_url, headers={'User-Agent': "LoginRadius - Python - SDK"})
                self.profile = json.loads(json_data)
                provider = self.profile['Provider']
                mapping = self.mappings.get(provider, self.mappings['default'])
                user = mapping(self.profile)
            except (JSONDecodeError, KeyError):
                pass
            if user is None and self.on_login_failure:
                redirect(self.on_login_failure)
        return user

    def login_form(self):
        loginradius_url = "https://hub.loginradius.com/include/js/LoginRadius.js"
        loginradius_lib = SCRIPT(_src=loginradius_url, _type='text/javascript')
        container = DIV(_id="interfacecontainerdiv", _class='interfacecontainerdiv')
        widget = SCRIPT("""var options={}; options.login=true;
        LoginRadius_SocialLogin.util.ready(function () {
        $ui = LoginRadius_SocialLogin.lr_login_settings;
        $ui.interfacesize = "";$ui.apikey = "%s";
        $ui.callback="%s"; $ui.lrinterfacecontainer ="interfacecontainerdiv";
        LoginRadius_SocialLogin.init(options); });""" % (self.api_key, self.url))
        form = DIV(container, loginradius_lib, widget)
        return form


def use_loginradius(auth, filename='private/loginradius.key', **kwargs):
    path = os.path.join(current.request.folder, filename)
    if os.path.exists(path):
        request = current.request
        domain, public_key, private_key = open(path, 'r').read().strip().split(':')
        url = URL('default', 'user', args='login', scheme=True)
        auth.settings.actions_disabled = \
            ['register', 'change_password', 'request_reset_password']
        auth.settings.login_form = LoginRadiusAccount(
            request, api_key=public_key, api_secret=private_key,
            url=url, **kwargs)
