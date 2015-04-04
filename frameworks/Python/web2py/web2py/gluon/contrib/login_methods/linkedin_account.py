#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of web2py Web Framework (Copyrighted, 2007-2009).
Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu>.
License: GPL v2

Thanks to Hans Donner <hans.donner@pobox.com> for GaeGoogleAccount.
"""

from gluon.http import HTTP
try:
    import linkedin
except ImportError:
    raise HTTP(400, "linkedin module not found")


class LinkedInAccount(object):
    """
    Login will be done via Google's Appengine login object, instead of web2py's
    login form.

    Include in your model (eg db.py)::

        from gluon.contrib.login_methods.linkedin_account import LinkedInAccount
        auth.settings.login_form=LinkedInAccount(request,KEY,SECRET,RETURN_URL)

    """

    def __init__(self, request, key, secret, return_url):
        self.request = request
        self.api = linkedin.LinkedIn(key, secret, return_url)
        self.token = result = self.api.requestToken()

    def login_url(self, next="/"):
        return self.api.getAuthorizeURL(self.token)

    def logout_url(self, next="/"):
        return ''

    def get_user(self):
        result = self.request.vars.verifier and self.api.accessToken(
            verifier=self.request.vars.verifier)
        if result:
            profile = self.api.GetProfile()
            profile = self.api.GetProfile(
                profile).public_url = "http://www.linkedin.com/in/ozgurv"
            return dict(first_name=profile.first_name,
                        last_name=profile.last_name,
                        username=profile.id)
