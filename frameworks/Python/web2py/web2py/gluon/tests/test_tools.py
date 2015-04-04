#!/bin/python
# -*- coding: utf-8 -*-

"""
    Unit tests for gluon.tools
"""
import os
import sys
if sys.version < "2.7":
    import unittest2 as unittest
else:
    import unittest

from fix_path import fix_sys_path

fix_sys_path(__file__)

DEFAULT_URI = os.getenv('DB', 'sqlite:memory')

from gluon.dal import DAL, Field
from pydal.objects import Table
from tools import Auth
from gluon.globals import Request, Response, Session
from storage import Storage
from languages import translator
from gluon.http import HTTP

python_version = sys.version[:3]
IS_IMAP = "imap" in DEFAULT_URI

@unittest.skipIf(IS_IMAP, "TODO: Imap raises 'Connection refused'")
class testAuth(unittest.TestCase):

    def testRun(self):
        # setup
        request = Request(env={})
        request.application = 'a'
        request.controller = 'c'
        request.function = 'f'
        request.folder = 'applications/admin'
        response = Response()
        session = Session()
        T = translator('', 'en')
        session.connect(request, response)
        from gluon.globals import current
        current.request = request
        current.response = response
        current.session = session
        current.T = T
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        auth = Auth(db)
        auth.define_tables(username=True, signature=False)
        self.assertTrue('auth_user' in db)
        self.assertTrue('auth_group' in db)
        self.assertTrue('auth_membership' in db)
        self.assertTrue('auth_permission' in db)
        self.assertTrue('auth_event' in db)
        db.define_table('t0', Field('tt'), auth.signature)
        auth.enable_record_versioning(db)
        self.assertTrue('t0_archive' in db)
        for f in ['login', 'register', 'retrieve_password',
                  'retrieve_username']:
            html_form = getattr(auth, f)().xml()
            self.assertTrue('name="_formkey"' in html_form)

        for f in ['logout', 'verify_email', 'reset_password',
                  'change_password', 'profile', 'groups']:
            self.assertRaisesRegexp(HTTP, "303*", getattr(auth, f))

        self.assertRaisesRegexp(HTTP, "401*", auth.impersonate)

        try:
            for t in ['t0_archive', 't0', 'auth_cas', 'auth_event',
                      'auth_membership', 'auth_permission', 'auth_group',
                      'auth_user']:
                db[t].drop()
        except SyntaxError as e:
            # GAE doesn't support drop
            pass
        return

if __name__ == '__main__':
    unittest.main()
