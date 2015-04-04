#!/usr/bin/env python
# coding: utf8

"""
ExtendedLoginForm is used to extend normal login form in web2py with one more login method.
So user can choose the built-in login or extended login methods.
"""

from gluon import current, DIV


class ExtendedLoginForm(object):
    """
    Put extended_login_form under web2py/gluon/contrib/login_methods folder.
    Then inside your model where defines the auth:

    auth = Auth(globals(),db)              # authentication/authorization
    ...
    auth.define_tables()                   # You might like to put the code after auth.define_tables
    ...                                    # if the alt_login_form deals with tables of auth.

    alt_login_form = RPXAccount(request,
                                api_key="...",
                                domain="...",
                                url = "http://localhost:8000/%s/default/user/login" % request.application)
    extended_login_form = ExtendedLoginForm(
        auth, alt_login_form, signals=['token'])

    auth.settings.login_form = extended_login_form

    Note:
        Since rpx_account doesn't create the password for the user, you
        might need to provide a way for user to create password to do
        normal login.

    """

    def __init__(self,
                 auth,
                 alt_login_form,
                 signals=[],
                 login_arg='login'
                 ):
        self.auth = auth
        self.alt_login_form = alt_login_form
        self.signals = signals
        self.login_arg = login_arg

    def get_user(self):
        """
        Delegate the get_user to alt_login_form.get_user.
        """
        if hasattr(self.alt_login_form, 'get_user'):
            return self.alt_login_form.get_user()
        return None  # let gluon.tools.Auth.get_or_create_user do the rest

    def login_url(self, next):
        """
        Optional implement for alt_login_form.

        In normal case, this should be replaced by get_user, and never get called.
        """
        if hasattr(self.alt_login_form, 'login_url'):
            return self.alt_login_form.login_url(next)
        return self.auth.settings.login_url

    def logout_url(self, next):
        """
        Optional implement for alt_login_form.

        Called if bool(alt_login_form.get_user) is True.

        If alt_login_form implemented logout_url function, it will return that function call.
        """
        if hasattr(self.alt_login_form, 'logout_url'):
            return self.alt_login_form.logout_url(next)
        return next

    def login_form(self):
        """
        Combine the auth() form with alt_login_form.

        If signals are set and a parameter in request matches any signals,
        it will return the call of alt_login_form.login_form instead.
        So alt_login_form can handle some particular situations, for example,
        multiple steps of OpenID login inside alt_login_form.login_form.

        Otherwise it will render the normal login form combined with
        alt_login_form.login_form.
        """

        request = current.request
        args = request.args

        if (self.signals and
            any([True for signal in self.signals if signal in request.vars])
            ):
            return self.alt_login_form.login_form()

        self.auth.settings.login_form = self.auth
        form = DIV(self.auth())
        self.auth.settings.login_form = self

        form.components.append(self.alt_login_form.login_form())
        return form
