#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of web2py Web Framework (Copyrighted, 2007-2009).
Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu>.
License: LGPL v3

Thanks to Hans Donner <hans.donner@pobox.com> for GaeGoogleAccount.
"""

from google.appengine.api import users


class GaeGoogleAccount(object):
    """
    Login will be done via Google's Appengine login object, instead of web2py's
    login form.

    Include in your model (eg db.py)::

        from gluon.contrib.login_methods.gae_google_account import \
            GaeGoogleAccount
        auth.settings.login_form=GaeGoogleAccount()

    """

    def login_url(self, next="/"):
        return users.create_login_url(next)

    def logout_url(self, next="/"):
        return users.create_logout_url(next)

    def get_user(self):
        user = users.get_current_user()
        if user:
            return dict(nickname = user.nickname(),
                        email = user.email(),
                        registration_id = user.user_id(),
                        user_id = user.user_id(),
                        source = "google account")
