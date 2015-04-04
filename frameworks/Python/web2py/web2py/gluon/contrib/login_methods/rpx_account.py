#!/usr/bin/env python
# coding: utf8

"""
   RPX Authentication for web2py
   Developed by Nathan Freeze (Copyright © 2009)
   Email <nathan@freezable.com>
   Modified by Massimo Di Pierro

   This file contains code to allow using RPXNow.com (now Jainrain.com)
   services with web2py
"""

import os
import re
import urllib
from gluon import *
from gluon.tools import fetch
from gluon.storage import Storage
import gluon.contrib.simplejson as json


class RPXAccount(object):

    """
    from gluon.contrib.login_methods.rpx_account import RPXAccount
    auth.settings.actions_disabled=['register','change_password',
        'request_reset_password']
    auth.settings.login_form = RPXAccount(request,
              api_key="...",
              domain="...",
              url = "http://localhost:8000/%s/default/user/login" % request.application)
    """

    def __init__(self,
                 request,
                 api_key="",
                 domain="",
                 url="",
                 embed=True,
                 auth_url="https://rpxnow.com/api/v2/auth_info",
                 language="en",
                 prompt='rpx',
                 on_login_failure=None,
                 ):

        self.request = request
        self.api_key = api_key
        self.embed = embed
        self.auth_url = auth_url
        self.domain = domain
        self.token_url = url
        self.language = language
        self.profile = None
        self.prompt = prompt
        self.on_login_failure = on_login_failure
        self.mappings = Storage()

        dn = {'givenName': '', 'familyName': ''}
        self.mappings.Facebook = lambda profile, dn=dn:\
            dict(registration_id=profile.get("identifier", ""),
                 username=profile.get("preferredUsername", ""),
                 email=profile.get("email", ""),
                 first_name=profile.get("name", dn).get("givenName", ""),
                 last_name=profile.get("name", dn).get("familyName", ""))
        self.mappings.Google = lambda profile, dn=dn:\
            dict(registration_id=profile.get("identifier", ""),
                 username=profile.get("preferredUsername", ""),
                 email=profile.get("email", ""),
                 first_name=profile.get("name", dn).get("givenName", ""),
                 last_name=profile.get("name", dn).get("familyName", ""))
        self.mappings.default = lambda profile:\
            dict(registration_id=profile.get("identifier", ""),
                 username=profile.get("preferredUsername", ""),
                 email=profile.get("email", ""),
                 first_name=profile.get("preferredUsername", ""),
                 last_name='')

    def get_user(self):
        request = self.request
        if request.vars.token:
            user = Storage()
            data = urllib.urlencode(
                dict(apiKey=self.api_key, token=request.vars.token))
            auth_info_json = fetch(self.auth_url + '?' + data)
            auth_info = json.loads(auth_info_json)

            if auth_info['stat'] == 'ok':
                self.profile = auth_info['profile']
                provider = re.sub('[^\w\-]', '', self.profile['providerName'])
                user = self.mappings.get(
                    provider, self.mappings.default)(self.profile)
                return user
            elif self.on_login_failure:
                redirect(self.on_login_failure)
        return None

    def login_form(self):
        request = self.request
        args = request.args
        if self.embed:
            JANRAIN_URL = \
                "https://%s.rpxnow.com/openid/embed?token_url=%s&language_preference=%s"
            rpxform = IFRAME(
                _src=JANRAIN_URL % (
                    self.domain, self.token_url, self.language),
                _scrolling="no",
                _frameborder="no",
                _style="width:400px;height:240px;")
        else:
            JANRAIN_URL = \
                "https://%s.rpxnow.com/openid/v2/signin?token_url=%s"
            rpxform = DIV(SCRIPT(_src="https://rpxnow.com/openid/v2/widget",
                                 _type="text/javascript"),
                          SCRIPT("RPXNOW.overlay = true;",
                                 "RPXNOW.language_preference = '%s';" % self.language,
                                 "RPXNOW.realm = '%s';" % self.domain,
                                 "RPXNOW.token_url = '%s';" % self.token_url,
                                 "RPXNOW.show();",
                                 _type="text/javascript"))
        return rpxform


def use_janrain(auth, filename='private/janrain.key', **kwargs):
    path = os.path.join(current.request.folder, filename)
    if os.path.exists(path):
        request = current.request
        domain, key = open(path, 'r').read().strip().split(':')
        host = current.request.env.http_host
        url = URL('default', 'user', args='login', scheme=True)
        auth.settings.actions_disabled = \
            ['register', 'change_password', 'request_reset_password']
        auth.settings.login_form = RPXAccount(
            request, api_key=key, domain=domain, url=url, **kwargs)
