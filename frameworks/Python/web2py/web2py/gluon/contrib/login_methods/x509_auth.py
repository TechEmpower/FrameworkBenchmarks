#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Written by Michele Comitini <mcm@glisco.it>
License: LGPL v3

Adds support for x509 authentication.

"""

from gluon.globals import current
from gluon.storage import Storage
from gluon.http import HTTP, redirect

#requires M2Crypto
from M2Crypto import X509


class X509Auth(object):
    """
    Login using x509 cert from client.

    from gluon.contrib.login_methods.x509_auth import X509Account
    auth.settings.actions_disabled=['register','change_password',
                                    'request_reset_password','profile']
    auth.settings.login_form = X509Auth()

    """

    def __init__(self):
        self.request = current.request
        self.ssl_client_raw_cert = self.request.env.ssl_client_raw_cert

        # rebuild the certificate passed by the env
        # this is double work, but it is the only way
        # since we cannot access the web server ssl engine directly

        if self.ssl_client_raw_cert:

            x509 = X509.load_cert_string(
                self.ssl_client_raw_cert, X509.FORMAT_PEM)
            # extract it from the cert
            self.serial = self.request.env.ssl_client_serial or (
                '%x' % x509.get_serial_number()).upper()

            subject = x509.get_subject()

            # Reordering the subject map to a usable Storage map
            # this allows us a cleaner syntax:
            # cn = self.subject.cn
            self.subject = Storage(filter(None,
                                          map(lambda x:
                                              (x, map(lambda y:
                                                      y.get_data(
                                                      ).as_text(),
                                                      subject.get_entries_by_nid(subject.nid[x]))),
                                              subject.nid.keys())))

    def login_form(self, **args):
        raise HTTP(403, 'Login not allowed. No valid x509 crentials')

    def login_url(self, next="/"):
        raise HTTP(403, 'Login not allowed. No valid x509 crentials')

    def logout_url(self, next="/"):
        return next

    def get_user(self):
        '''Returns the user info contained in the certificate.
        '''

        # We did not get the client cert?
        if not self.ssl_client_raw_cert:
            return None

        # Try to reconstruct some useful info for web2py auth machinery

        p = profile = dict()

        username = p['username'] = reduce(lambda a, b: '%s | %s' % (
            a, b), self.subject.CN or self.subject.commonName)
        p['first_name'] = reduce(lambda a, b: '%s | %s' % (a, b),
                                 self.subject.givenName or username)
        p['last_name'] = reduce(
            lambda a, b: '%s | %s' % (a, b), self.subject.surname)
        p['email'] = reduce(lambda a, b: '%s | %s' % (
            a, b), self.subject.Email or self.subject.emailAddress)

        # IMPORTANT WE USE THE CERT SERIAL AS UNIQUE KEY FOR THE USER
        p['registration_id'] = self.serial

        # If the auth table has a field certificate it will be used to
        # save a PEM encoded copy of the user certificate.

        p['certificate'] = self.ssl_client_raw_cert

        return profile
