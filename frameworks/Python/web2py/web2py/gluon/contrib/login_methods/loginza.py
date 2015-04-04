#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
   Loginza.ru authentication for web2py
   Developed by Vladimir Dronnikov (Copyright © 2011)
   Email <dronnikov@gmail.com>
"""

import urllib
from gluon.html import *
from gluon.tools import fetch
from gluon.storage import Storage
import gluon.contrib.simplejson as json


class Loginza(object):

    """
    from gluon.contrib.login_methods.loginza import Loginza
    auth.settings.login_form = Loginza(request,
        url = "http://localhost:8000/%s/default/user/login" % request.application)
    """

    def __init__(self,
                 request,
                 url="",
                 embed=True,
                 auth_url="http://loginza.ru/api/authinfo",
                 language="en",
                 prompt="loginza",
                 on_login_failure=None,
                 ):

        self.request = request
        self.token_url = url
        self.embed = embed
        self.auth_url = auth_url
        self.language = language
        self.prompt = prompt
        self.profile = None
        self.on_login_failure = on_login_failure
        self.mappings = Storage()

        # TODO: profile.photo is the URL to the picture
        # Howto download and store it locally?
        # FIXME: what if email is unique=True

        self.mappings["http://twitter.com/"] = lambda profile:\
            dict(registration_id=profile.get("identity", ""),
                 username=profile.get("nickname", ""),
                 email=profile.get("email", ""),
                 last_name=profile.get("name", "").get("full_name", ""),
                 #avatar = profile.get("photo",""),
                 )
        self.mappings["https://www.google.com/accounts/o8/ud"] = lambda profile:\
            dict(registration_id=profile.get("identity", ""),
                 username=profile.get("name", "").get("full_name", ""),
                 email=profile.get("email", ""),
                 first_name=profile.get("name", "").get("first_name", ""),
                 last_name=profile.get("name", "").get("last_name", ""),
                 #avatar = profile.get("photo",""),
                 )
        self.mappings["http://vkontakte.ru/"] = lambda profile:\
            dict(registration_id=profile.get("identity", ""),
                 username=profile.get("name", "").get("full_name", ""),
                 email=profile.get("email", ""),
                 first_name=profile.get("name", "").get("first_name", ""),
                 last_name=profile.get("name", "").get("last_name", ""),
                 #avatar = profile.get("photo",""),
                 )
        self.mappings.default = lambda profile:\
            dict(registration_id=profile.get("identity", ""),
                 username=profile.get("name", "").get("full_name"),
                 email=profile.get("email", ""),
                 first_name=profile.get("name", "").get("first_name", ""),
                 last_name=profile.get("name", "").get("last_name", ""),
                 #avatar = profile.get("photo",""),
                 )

    def get_user(self):
        request = self.request
        if request.vars.token:
            user = Storage()
            data = urllib.urlencode(dict(token=request.vars.token))
            auth_info_json = fetch(self.auth_url + '?' + data)
            #print auth_info_json
            auth_info = json.loads(auth_info_json)
            if auth_info["identity"] is not None:
                self.profile = auth_info
                provider = self.profile["provider"]
                user = self.mappings.get(
                    provider, self.mappings.default)(self.profile)
                #user["password"] = ???
                #user["avatar"] = ???
                return user
            elif self.on_login_failure:
                redirect(self.on_login_failure)
        return None

    def login_form(self):
        request = self.request
        args = request.args
        LOGINZA_URL = "https://loginza.ru/api/widget?lang=%s&token_url=%s&overlay=loginza"
        if self.embed:
            form = IFRAME(_src=LOGINZA_URL % (self.language, self.token_url),
                          _scrolling="no",
                          _frameborder="no",
                          _style="width:359px;height:300px;")
        else:
            form = DIV(
                A(self.prompt, _href=LOGINZA_URL % (
                    self.language, self.token_url), _class="loginza"),
                SCRIPT(_src="https://s3-eu-west-1.amazonaws.com/s1.loginza.ru/js/widget.js", _type="text/javascript"))
        return form
