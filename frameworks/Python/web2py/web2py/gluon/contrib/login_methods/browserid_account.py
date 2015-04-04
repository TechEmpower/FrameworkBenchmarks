#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    BrowserID Authentication for web2py
    developed by Madhukar R Pai (Copyright 2012)
    Email <madspai@gmail.com>
    License : LGPL

    thanks and credits to the web2py community

    This custom authenticator allows web2py to authenticate using browserid (https://login.persona.org/)
    BrowserID is a project by Mozilla Labs (http://mozillalabs.com/)
    to Know how browserid works please visit http://identity.mozilla.com/post/7616727542/introducing-browserid-a-better-way-to-sign-in

    bottom line BrowserID provides a free, secure, de-centralized, easy to use(for users and developers) login solution.
    You can use any email id as your login id. Browserid just verifys the email id and lets you login with that id.

    credits for the doPost jquery function - itsadok (http://stackoverflow.com/users/7581/itsadok)

"""
import time
from gluon import *
from gluon.storage import Storage
from gluon.tools import fetch
import gluon.contrib.simplejson as json


class BrowserID(object):
    """
    from gluon.contrib.login_methods.browserid_account import BrowserID
    auth.settings.login_form = BrowserID(request,
        audience = "http://127.0.0.1:8000"
        assertion_post_url = "http://127.0.0.1:8000/%s/default/user/login" % request.application)
    """

    def __init__(self,
                 request,
                 audience="",
                 assertion_post_url="",
                 prompt="BrowserID Login",
                 issuer="login.persona.org",
                 verify_url="https://login.persona.org/verify",
                 browserid_js="https://login.persona.org/include.js",
                 browserid_button="https://login.persona.org/i/sign_in_red.png",
                 crypto_js="https://crypto-js.googlecode.com/files/2.2.0-crypto-md5.js",
                 on_login_failure=None,
                 ):

        self.request = request
        self.audience = audience
        self.assertion_post_url = assertion_post_url
        self.prompt = prompt
        self.issuer = issuer
        self.verify_url = verify_url
        self.browserid_js = browserid_js
        self.browserid_button = browserid_button
        self.crypto_js = crypto_js
        self.on_login_failure = on_login_failure
        self.asertion_js = """
            (function($){$.extend({doPost:function(url,params){var $form=$("<form method='POST'>").attr("action",url);
            $.each(params,function(name,value){$("<input type='hidden'>").attr("name",name).attr("value",value).appendTo($form)});
            $form.appendTo("body");$form.submit()}})})(jQuery);
            function gotVerifiedEmail(assertion){if(assertion !== null){$.doPost('%s',{'assertion':assertion});}}""" % self.assertion_post_url

    def get_user(self):
        request = self.request
        if request.vars.assertion:
            audience = self.audience
            issuer = self.issuer
            assertion = XML(request.vars.assertion, sanitize=True)
            verify_data = {'assertion': assertion, 'audience': audience}
            auth_info_json = fetch(self.verify_url, data=verify_data)
            j = json.loads(auth_info_json)
            epoch_time = int(time.time() * 1000)  # we need 13 digit epoch time
            if j["status"] == "okay" and j["audience"] == audience and j['issuer'].endswith(issuer) and j['expires'] >= epoch_time:
                return dict(email=j['email'])
            elif self.on_login_failure:
                #print "status:  ", j["status"]=="okay", j["status"]
                #print "audience:", j["audience"]==audience, j["audience"], audience
                #print "issuer:  ", j["issuer"]==issuer, j["issuer"], issuer
                #print "expires:  ", j["expires"] >= epoch_time, j["expires"], epoch_time
                redirect(self.on_login_failure)
            else:
                redirect('https://login.persona.org')
        return None

    def login_form(self):
        request = self.request
        onclick = "javascript:navigator.id.getVerifiedEmail(gotVerifiedEmail) ; return false"
        form = DIV(SCRIPT(_src=self.browserid_js, _type="text/javascript"),
                   SCRIPT(_src=self.crypto_js, _type="text/javascript"),
                   A(IMG(_src=self.browserid_button, _alt=self.prompt), _href="#", _onclick=onclick, _class="browserid", _title="Login With BrowserID"),
                   SCRIPT(self.asertion_js))
        return form
