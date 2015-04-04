#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Unit tests for rewrite.py routers option"""

import sys
import os
import unittest
import tempfile
import logging

if os.path.isdir('gluon'):
    sys.path.insert(0,os.path.realpath('gluon'))  # running from web2py base
else:
    sys.path.insert(0,os.path.realpath('../../'))  # running from gluon/tests/
    os.environ['web2py_path'] = os.path.realpath('../../')  # for settings

from gluon.rewrite import load, filter_url, filter_err, get_effective_router, map_url_out
from gluon.html import URL
from gluon.fileutils import abspath
from gluon.settings import global_settings
from gluon.http import HTTP
from gluon.storage import Storage

logger = None
oldcwd = None
root = None

def norm_root(root):
    return root.replace('/', os.sep)

def setUpModule():
    def make_apptree():
        "build a temporary applications tree"
        #  applications/
        os.mkdir(abspath('applications'))

        #  applications/app/
        for app in ('admin', 'examples', 'welcome'):
            os.mkdir(abspath('applications', app))
            #  applications/app/(controllers, static)
            for subdir in ('controllers', 'static'):
                os.mkdir(abspath('applications', app, subdir))

        #  applications/admin/controllers/*.py
        for ctr in ('appadmin', 'default', 'gae', 'mercurial', 'shell', 'wizard'):
            open(abspath('applications', 'admin',
                 'controllers', '%s.py' % ctr), 'w').close()
        #  applications/examples/controllers/*.py
        for ctr in ('ajax_examples', 'appadmin', 'default', 'global', 'spreadsheet'):
            open(abspath('applications', 'examples',
                 'controllers', '%s.py' % ctr), 'w').close()
        #  applications/welcome/controllers/*.py
        #  (include controller that collides with another app)
        for ctr in ('appadmin', 'default', 'other', 'admin'):
            open(abspath('applications', 'welcome',
                 'controllers', '%s.py' % ctr), 'w').close()

        #  create an app-specific routes.py for examples app
        routes = open(abspath('applications', 'examples', 'routes.py'), 'w')
        routes.write("routers=dict(examples=dict(default_function='exdef'))")
        routes.close()

        #  create language files for examples app
        for lang in ('en', 'it'):
            os.mkdir(abspath('applications', 'examples', 'static', lang))
            open(abspath('applications', 'examples', 'static',
                 lang, 'file'), 'w').close()

    global oldcwd
    if oldcwd is None:  # do this only once
        oldcwd = os.getcwd()
        if not os.path.isdir('gluon'):
            os.chdir(os.path.realpath(
                '../../'))    # run from web2py base directory
        import gluon.main     # for initialization after chdir
        global logger
        logger = logging.getLogger('web2py.rewrite')
        global_settings.applications_parent = tempfile.mkdtemp()
        global root
        root = global_settings.applications_parent
        make_apptree()


def tearDownModule():
    global oldcwd
    if oldcwd is not None:
        os.chdir(oldcwd)
        oldcwd = None


class TestRouter(unittest.TestCase):
    """ Tests the routers logic from gluon.rewrite """

    def test_router_syntax(self):
        """ Test router syntax error """
        level = logger.getEffectiveLevel()
        logger.setLevel(logging.CRITICAL)  # disable logging temporarily
        self.assertRaises(SyntaxError, load, data='x:y')
        self.assertRaises(
            SyntaxError, load, rdict=dict(BASE=dict(badkey="value")))
        self.assertRaises(SyntaxError, load, rdict=dict(
            BASE=dict(), app=dict(default_application="name")))
        try:
            # 2.7+ only
            self.assertRaisesRegexp(SyntaxError, "invalid syntax",
                                    load, data='x:y')
            self.assertRaisesRegexp(SyntaxError, "unknown key",
                                    load, rdict=dict(BASE=dict(badkey="value")))
            self.assertRaisesRegexp(SyntaxError, "BASE-only key",
                                    load, rdict=dict(BASE=dict(), app=dict(default_application="name")))
        except AttributeError:
            pass
        logger.setLevel(level)

    def test_router_null(self):
        """ Tests the null router """
        load(rdict=dict())
        # app resolution
        self.assertEqual(
            filter_url('http://domain.com/welcome', app=True), 'welcome')
        self.assertEqual(filter_url('http://domain.com/', app=True), 'init')
        # incoming
        self.assertEqual(filter_url('http://domain.com/favicon.ico'),
                         norm_root('%s/applications/init/static/favicon.ico' % root))
        self.assertEqual(
            filter_url('http://domain.com/abc'), '/init/default/abc')
        self.assertEqual(filter_url(
            'http://domain.com/index/abc'), "/init/default/index ['abc']")
        self.assertEqual(filter_url(
            'http://domain.com/abc/def'), "/init/default/abc ['def']")
        self.assertEqual(filter_url(
            'http://domain.com/index/a%20bc'), "/init/default/index ['a bc']")
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/st~tic')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*invalid static file", filter_url, 'http://domain.com/welcome/static/bad/path/to/st~tic')
        except AttributeError:
            pass
        # outgoing
        self.assertEqual(
            filter_url('http://domain.com/init/default/index', out=True), '/')
        self.assertEqual(filter_url('http://domain.com/init/default/index/arg1', out=True), '/index/arg1')
        self.assertEqual(filter_url(
            'http://domain.com/init/default/abc', out=True), '/abc')
        self.assertEqual(filter_url('http://domain.com/init/static/abc',
                         out=True), '/init/static/abc')
        self.assertEqual(filter_url(
            'http://domain.com/init/appadmin/index', out=True), '/appadmin')
        self.assertEqual(filter_url(
            'http://domain.com/init/appadmin/abc', out=True), '/appadmin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/init/admin/index', out=True), '/init/admin')
        self.assertEqual(filter_url(
            'http://domain.com/init/admin/abc', out=True), '/init/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')

    def test_router_specific(self):
        """
        Test app-specific routes.py

        Note that make_apptree above created applications/examples/routes.py with a default_function.
        """
        load(rdict=dict())
        self.assertEqual(
            filter_url('http://domain.com/welcome'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain.com/examples'), '/examples/default/exdef')

    def test_router_defapp(self):
        """ Test the default-application function """
        routers = dict(BASE=dict(default_application='welcome'))
        load(rdict=routers)
        # app resolution
        self.assertEqual(
            filter_url('http://domain.com/welcome', app=True), 'welcome')
        self.assertEqual(filter_url('http://domain.com/', app=True), 'welcome')
        # incoming
        self.assertEqual(
            filter_url('http://domain.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain.com/'), '/welcome/default/index')
        self.assertEqual(filter_url(
            'http://domain.com/appadmin'), '/welcome/appadmin/index')
        self.assertEqual(
            filter_url('http://domain.com/abc'), '/welcome/default/abc')
        self.assertEqual(filter_url(
            'http://domain.com/index/abc'), "/welcome/default/index ['abc']")
        self.assertEqual(filter_url(
            'http://domain.com/abc/def'), "/welcome/default/abc ['def']")
        self.assertEqual(filter_url('http://domain.com/favicon.ico'),
                         norm_root('%s/applications/welcome/static/favicon.ico' % root))
        self.assertEqual(filter_url('http://domain.com/static/abc'),
                         norm_root('%s/applications/welcome/static/abc' % root))
        self.assertEqual(filter_url('http://domain.com/static/path/to/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/static" % root))
        # outgoing
        self.assertEqual(filter_url(
            'http://domain.com/welcome/default/index', out=True), '/')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/index/arg1')
        self.assertEqual(filter_url(
            'http://domain.com/welcome/default/abc', out=True), '/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/default/admin',
                         out=True), '/default/admin')
        self.assertEqual(
            filter_url('http://domain.com/welcome/static/abc', out=True),
            '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')

    def test_router_nodef(self):
        """ Test no-default functions """
        routers = dict(
            BASE=dict(default_application='welcome'),
            welcome=dict(controllers=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url(
            'http://domain.com/welcome/default/index', out=True), '/default')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/default/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/default/abc')
        self.assertEqual(
            filter_url('http://domain.com/welcome/static/abc', out=True),
            '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertEqual(
            filter_url('http://domain.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain.com/'), '/welcome/default/index')
        self.assertEqual(filter_url(
            'http://domain.com/appadmin'), '/welcome/appadmin/index')
        self.assertEqual(
            filter_url('http://domain.com/abc'), '/welcome/abc/index')
        self.assertEqual(
            filter_url('http://domain.com/index/abc'), "/welcome/index/abc")
        self.assertEqual(
            filter_url('http://domain.com/abc/def'), "/welcome/abc/def")
        self.assertEqual(filter_url(
            'http://domain.com/abc/def/ghi'), "/welcome/abc/def ['ghi']")

        routers = dict(
            BASE=dict(default_application=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url(
            'http://domain.com/welcome/default/index', out=True), '/welcome')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/welcome/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/welcome/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/static/abc',
                         out=True), '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/welcome/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/welcome/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertRaises(HTTP, filter_url, 'http://domain.com')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/appadmin')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*invalid application",
                                    filter_url, 'http://domain.com')
            self.assertRaisesRegexp(HTTP, "400.*invalid application",
                                    filter_url, 'http://domain.com/appadmin')
        except AttributeError:
            pass

        routers = dict(
            BASE=dict(default_application='welcome', applications=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url(
            'http://domain.com/welcome/default/index', out=True), '/welcome')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/welcome/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/welcome/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/static/abc',
                         out=True), '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/welcome/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/welcome/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertEqual(
            filter_url('http://domain.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain.com/'), '/welcome/default/index')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/appadmin')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*unknown application: 'appadmin'", filter_url, 'http://domain.com/appadmin')
        except AttributeError:
            pass

        routers = dict(
            BASE=dict(default_application='welcome', applications=None),
            welcome=dict(controllers=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url('http://domain.com/welcome/default/index',
                         out=True), '/welcome/default')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/welcome/default/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/welcome/default/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/static/abc',
                         out=True), '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/welcome/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/welcome/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertEqual(
            filter_url('http://domain.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain.com/'), '/welcome/default/index')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/appadmin')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*unknown application: 'appadmin'", filter_url, 'http://domain.com/appadmin')
        except AttributeError:
            pass

        routers = dict(
            BASE=dict(default_application='welcome', applications=None),
            welcome=dict(default_controller=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url('http://domain.com/welcome/default/index',
                         out=True), '/welcome/default')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/welcome/default/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/welcome/default/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/static/abc',
                         out=True), '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/welcome/appadmin')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/welcome/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertRaises(HTTP, filter_url, 'http://domain.com')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/appadmin')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*invalid controller",
                                    filter_url, 'http://domain.com')
            self.assertRaisesRegexp(HTTP, "400.*unknown application: 'appadmin'", filter_url, 'http://domain.com/appadmin')
        except AttributeError:
            pass

        routers = dict(
            BASE=dict(default_application='welcome', applications=None),
            welcome=dict(controllers=None, default_function=None),
        )
        load(rdict=routers)
        # outgoing
        self.assertEqual(filter_url('http://domain.com/welcome/default/index',
                         out=True), '/welcome/default/index')
        self.assertEqual(filter_url('http://domain.com/welcome/default/index/arg1', out=True), '/welcome/default/index/arg1')
        self.assertEqual(filter_url('http://domain.com/welcome/default/abc',
                         out=True), '/welcome/default/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/static/abc',
                         out=True), '/welcome/static/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/index',
                         out=True), '/welcome/appadmin/index')
        self.assertEqual(filter_url('http://domain.com/welcome/appadmin/abc',
                         out=True), '/welcome/appadmin/abc')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/index',
                         out=True), '/welcome/admin/index')
        self.assertEqual(filter_url('http://domain.com/welcome/admin/abc',
                         out=True), '/welcome/admin/abc')
        self.assertEqual(filter_url(
            'http://domain.com/admin/default/abc', out=True), '/admin/abc')
        # incoming
        self.assertRaises(HTTP, filter_url, 'http://domain.com')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/appadmin')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, "400.*invalid function",
                                    filter_url, 'http://domain.com')
            self.assertRaisesRegexp(HTTP, "400.*unknown application: 'appadmin'", filter_url, 'http://domain.com/appadmin')
        except AttributeError:
            pass

    def test_router_app(self):
        """ Tests the doctest router app resolution"""
        routers = dict(
            BASE=dict(
                domains={
                    "domain1.com": "app1",
                    "www.domain1.com": "app1",
                    "domain2.com": "app2",
                },
            ),
            app1=dict(),
            app2=dict(),
            goodapp=dict(),
        )
        routers['bad!app'] = dict()
        load(rdict=routers)
        self.assertEqual(
            filter_url('http://domain.com/welcome', app=True), 'welcome')
        self.assertEqual(
            filter_url('http://domain.com/welcome/', app=True), 'welcome')
        self.assertEqual(filter_url('http://domain.com', app=True), 'init')
        self.assertEqual(filter_url('http://domain.com/', app=True), 'init')
        self.assertEqual(filter_url('http://domain.com/abc', app=True), 'init')
        self.assertEqual(
            filter_url('http://domain1.com/abc', app=True), 'app1')
        self.assertEqual(
            filter_url('http://www.domain1.com/abc', app=True), 'app1')
        self.assertEqual(
            filter_url('http://domain2.com/abc', app=True), 'app2')
        self.assertEqual(
            filter_url('http://domain2.com/admin', app=True), 'admin')

        routers['BASE']['exclusive_domain'] = True
        load(rdict=routers)
        self.assertEqual(
            filter_url('http://domain2.com/admin', app=True), 'app2')

        self.assertEqual(
            filter_url('http://domain.com/goodapp', app=True), 'goodapp')
        self.assertRaises(
            HTTP, filter_url, 'http://domain.com/bad!app', app=True)
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, '400.*invalid application',
                                    filter_url, 'http://domain.com/bad!app')
        except AttributeError:
            pass

        routers['BASE']['domains']['domain3.com'] = 'app3'
        self.assertRaises(SyntaxError, load, rdict=routers)
        try:
            # 2.7+ only
            self.assertRaisesRegexp(
                SyntaxError, "unknown.*app3", load, rdict=routers)
        except AttributeError:
            pass

    def test_router_domains_fs(self):
        '''
        Test URLs that map domains using test filesystem layout
        '''
        routers = dict(
            BASE=dict(
                domains={
                    "domain1.com": "admin",
                    "domain2.com": "welcome",
                },
            ),
        )

        load(rdict=routers)
        self.assertEqual(
            filter_url('http://domain1.com'), '/admin/default/index')
        self.assertEqual(
            filter_url('http://domain2.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://domain1.com/gae'), '/admin/gae/index')
        self.assertEqual(
            filter_url('http://domain2.com/other'), '/welcome/other/index')
        self.assertEqual(
            filter_url('http://domain1.com/gae/f1'), '/admin/gae/f1')
        self.assertEqual(
            filter_url('http://domain2.com/f2'), '/welcome/default/f2')
        self.assertEqual(
            filter_url('http://domain2.com/other/f3'), '/welcome/other/f3')

    def test_router_domains(self):
        '''
        Test URLs that map domains
        '''
        routers = dict(
            BASE=dict(
                applications=['app1', 'app2', 'app2A',
                              'app3', 'app4', 'app5', 'app6'],
                domains={
                    #  two domains to the same app
                    "domain1.com": "app1",
                    "www.domain1.com": "app1",
                    #  same domain, two ports, to two apps
                    "domain2.com": "app2a",
                    "domain2.com:8080": "app2b",
                    #  two domains, same app, two controllers
                    "domain3a.com": "app3/c3a",
                    "domain3b.com": "app3/c3b",
                    #  two domains, same app & controller, two functions
                    "domain4a.com": "app4/c4/f4a",
                    "domain4b.com": "app4/c4/f4b",
                    #  http vs https
                    "domain6.com:80": "app6",
                    "domain6.com:443": "app6s",
                },
            ),
            app1=dict(default_controller='c1', default_function='f1',
                      controllers=['c1'], exclusive_domain=True, ),
            app2a=dict(default_controller='c2a',
                       default_function='f2a', controllers=['c2a'], ),
            app2b=dict(default_controller='c2b',
                       default_function='f2b', controllers=['c2b'], ),
            app3=dict(controllers=['c3a', 'c3b'], ),
            app4=dict(default_controller='c4', controllers=['c4']),
            app5=dict(default_controller='c5',
                      controllers=['c5'], domain='localhost'),
            app6=dict(default_controller='c6',
                      default_function='f6', controllers=['c6'], ),
            app6s=dict(default_controller='c6s',
                       default_function='f6s', controllers=['c6s'], ),
        )

        load(rdict=routers)
        self.assertEqual(filter_url('http://domain1.com/abc'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/c1/abc'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/abc.html'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/abc.css'), '/app1/c1/abc.css')
        self.assertEqual(filter_url(
            'http://domain1.com/index/abc'), "/app1/c1/index ['abc']")
        self.assertEqual(filter_url('http://domain2.com/app1'), "/app1/c1/f1")

        self.assertEqual(filter_url('https://domain1.com/app1/ctr/fcn',
                         domain=('app1', None), out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://www.domain1.com/app1/ctr/fcn',
                         domain=('app1', None), out=True), "/ctr/fcn")

        self.assertEqual(
            filter_url('http://domain2.com/abc'), '/app2a/c2a/abc')
        self.assertEqual(
            filter_url('http://domain2.com:8080/abc'), '/app2b/c2b/abc')

        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/fcn',
                         domain=('app2a', None), out=True), "/ctr/fcn")
        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/f2a',
                         domain=('app2a', None), out=True), "/ctr")
        self.assertEqual(filter_url('http://domain2.com/app2a/c2a/f2a',
                         domain=('app2a', None), out=True), "/")
        self.assertEqual(filter_url('http://domain2.com/app2a/c2a/fcn',
                         domain=('app2a', None), out=True), "/fcn")
        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/fcn',
                         domain=('app2b', None), out=True), "/app2a/ctr/fcn")
        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/f2a',
                         domain=('app2b', None), out=True), "/app2a/ctr")
        self.assertEqual(filter_url('http://domain2.com/app2a/c2a/f2a',
                         domain=('app2b', None), out=True), "/app2a")

        self.assertEqual(filter_url('http://domain3a.com/'), '/app3/c3a/index')
        self.assertEqual(
            filter_url('http://domain3a.com/abc'), '/app3/c3a/abc')
        self.assertEqual(
            filter_url('http://domain3a.com/c3b'), '/app3/c3b/index')
        self.assertEqual(
            filter_url('http://domain3b.com/abc'), '/app3/c3b/abc')

        self.assertEqual(filter_url('http://domain3a.com/app3/c3a/fcn',
                         domain=('app3', 'c3a'), out=True), "/fcn")
        self.assertEqual(filter_url('http://domain3a.com/app3/c3a/fcn',
                         domain=('app3', 'c3b'), out=True), "/c3a/fcn")
        self.assertEqual(filter_url('http://domain3a.com/app3/c3a/fcn',
                         domain=('app1', None), out=True), "/app3/c3a/fcn")

        self.assertEqual(filter_url('http://domain4a.com/abc'), '/app4/c4/abc')
        self.assertEqual(filter_url('https://domain4a.com/app4/c4/fcn',
                         domain=('app4', None), out=True), "/fcn")

        self.assertEqual(filter_url('http://domain4a.com'), '/app4/c4/f4a')
        self.assertEqual(filter_url('http://domain4b.com'), '/app4/c4/f4b')

        self.assertEqual(filter_url('http://localhost/abc'), '/app5/c5/abc')
        self.assertEqual(filter_url(
            'http:///abc'), '/app5/c5/abc')  # test null host => localhost
        self.assertEqual(filter_url('https://localhost/app5/c5/fcn',
                         domain=('app5', None), out=True), "/fcn")

        self.assertEqual(filter_url('http://domain6.com'), '/app6/c6/f6')
        self.assertEqual(filter_url('https://domain6.com'), '/app6s/c6s/f6s')

        self.assertEqual(filter_url('http://domain2.com/app3/c3a/f3',
                         domain=('app2b', None), out=True), "/app3/c3a/f3")
        self.assertRaises(SyntaxError, filter_url, 'http://domain1.com/app1/c1/f1', domain=('app2b', None), out=True)
        try:
            # 2.7+ only
            self.assertRaisesRegexp(SyntaxError, 'cross-domain conflict', filter_url, 'http://domain1.com/app1/c1/f1', domain=('app2b', None), out=True)
        except AttributeError:
            pass
        self.assertEqual(filter_url('http://domain1.com/app1/c1/f1', domain=(
            'app2b', None), host='domain2.com', out=True), "/app1")

    def test_router_domains_ed(self):
        '''
        Test URLs that map domains with exclusive_domain set
        '''
        routers = dict(
            BASE=dict(
                applications=['app1', 'app2', 'app2A',
                              'app3', 'app4', 'app5', 'app6'],
                exclusive_domain=True,
                domains={
                    #  two domains to the same app
                    "domain1.com": "app1",
                    "www.domain1.com": "app1",
                    #  same domain, two ports, to two apps
                    "domain2.com": "app2a",
                    "domain2.com:8080": "app2b",
                    #  two domains, same app, two controllers
                    "domain3a.com": "app3/c3a",
                    "domain3b.com": "app3/c3b",
                    #  two domains, same app & controller, two functions
                    "domain4a.com": "app4/c4/f4a",
                    "domain4b.com": "app4/c4/f4b",
                    #  http vs https
                    "domain6.com:80": "app6",
                    "domain6.com:443": "app6s",
                },
            ),
            app1=dict(default_controller='c1', default_function='f1',
                      controllers=['c1'], exclusive_domain=True, ),
            app2a=dict(default_controller='c2a',
                       default_function='f2a', controllers=['c2a'], ),
            app2b=dict(default_controller='c2b',
                       default_function='f2b', controllers=['c2b'], ),
            app3=dict(controllers=['c3a', 'c3b'], ),
            app4=dict(default_controller='c4', controllers=['c4']),
            app5=dict(default_controller='c5',
                      controllers=['c5'], domain='localhost'),
            app6=dict(default_controller='c6',
                      default_function='f6', controllers=['c6'], ),
            app6s=dict(default_controller='c6s',
                       default_function='f6s', controllers=['c6s'], ),
        )

        load(rdict=routers)
        self.assertEqual(filter_url('http://domain1.com/abc'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/c1/abc'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/abc.html'), '/app1/c1/abc')
        self.assertEqual(
            filter_url('http://domain1.com/abc.css'), '/app1/c1/abc.css')
        self.assertEqual(filter_url(
            'http://domain1.com/index/abc'), "/app1/c1/index ['abc']")
        self.assertEqual(
            filter_url('http://domain2.com/app1'), "/app2a/c2a/app1")

        self.assertEqual(filter_url('https://domain1.com/app1/ctr/fcn',
                         domain=('app1', None), out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://www.domain1.com/app1/ctr/fcn',
                         domain=('app1', None), out=True), "/ctr/fcn")

        self.assertEqual(
            filter_url('http://domain2.com/abc'), '/app2a/c2a/abc')
        self.assertEqual(
            filter_url('http://domain2.com:8080/abc'), '/app2b/c2b/abc')

        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/fcn',
                         domain=('app2a', None), out=True), "/ctr/fcn")
        self.assertEqual(filter_url('http://domain2.com/app2a/ctr/f2a',
                         domain=('app2a', None), out=True), "/ctr")
        self.assertEqual(filter_url('http://domain2.com/app2a/c2a/f2a',
                         domain=('app2a', None), out=True), "/")
        self.assertEqual(filter_url('http://domain2.com/app2a/c2a/fcn',
                         domain=('app2a', None), out=True), "/fcn")

        self.assertRaises(SyntaxError, filter_url, 'http://domain2.com/app2a/ctr/fcn', domain=('app2b', None), out=True)
        self.assertRaises(SyntaxError, filter_url, 'http://domain2.com/app2a/ctr/f2a', domain=('app2b', None), out=True)
        self.assertRaises(SyntaxError, filter_url, 'http://domain2.com/app2a/c2a/f2a', domain=('app2b', None), out=True)

        self.assertEqual(filter_url('http://domain3a.com/'), '/app3/c3a/index')
        self.assertEqual(
            filter_url('http://domain3a.com/abc'), '/app3/c3a/abc')
        self.assertEqual(
            filter_url('http://domain3a.com/c3b'), '/app3/c3b/index')
        self.assertEqual(
            filter_url('http://domain3b.com/abc'), '/app3/c3b/abc')

        self.assertEqual(filter_url('http://domain3a.com/app3/c3a/fcn',
                         domain=('app3', 'c3a'), out=True), "/fcn")
        self.assertEqual(filter_url('http://domain3a.com/app3/c3a/fcn',
                         domain=('app3', 'c3b'), out=True), "/c3a/fcn")

        self.assertRaises(SyntaxError, filter_url, 'http://domain3a.com/app3/c3a/fcn', domain=('app1', None), out=True)

        self.assertEqual(filter_url('http://domain4a.com/abc'), '/app4/c4/abc')
        self.assertEqual(filter_url('https://domain4a.com/app4/c4/fcn',
                         domain=('app4', None), out=True), "/fcn")

        self.assertEqual(filter_url('http://domain4a.com'), '/app4/c4/f4a')
        self.assertEqual(filter_url('http://domain4b.com'), '/app4/c4/f4b')

        self.assertEqual(filter_url('http://localhost/abc'), '/app5/c5/abc')
        self.assertEqual(filter_url(
            'http:///abc'), '/app5/c5/abc')  # test null host => localhost
        self.assertEqual(filter_url('https://localhost/app5/c5/fcn',
                         domain=('app5', None), out=True), "/fcn")

        self.assertEqual(filter_url('http://domain6.com'), '/app6/c6/f6')
        self.assertEqual(filter_url('https://domain6.com'), '/app6s/c6s/f6s')

        self.assertRaises(SyntaxError, filter_url, 'http://domain2.com/app3/c3a/f3', domain=('app2b', None), out=True)
        self.assertRaises(SyntaxError, filter_url, 'http://domain1.com/app1/c1/f1', domain=('app2b', None), out=True)
        try:
            # 2.7+ only
            self.assertRaisesRegexp(SyntaxError, 'cross-domain conflict', filter_url, 'http://domain1.com/app1/c1/f1', domain=('app2b', None), out=True)
        except AttributeError:
            pass
        self.assertEqual(filter_url('http://domain1.com/app1/c1/f1', domain=(
            'app2b', None), host='domain2.com', out=True), "/app1")

    def test_router_raise(self):
        '''
        Test URLs that raise exceptions
        '''
        # test non-exception variants
        router_raise = dict(
            init=dict(
                controllers=[],
            ),
            welcome=dict(
                map_hyphen=False,
            ),
        )
        load(rdict=router_raise)
        self.assertEqual(
            filter_url('http://domain.com/ctl'), "/init/ctl/index")
        self.assertEqual(
            filter_url('http://domain.com/default/fcn'), "/init/default/fcn")
        self.assertEqual(filter_url(
            'http://domain.com/default/fcn.ext'), "/init/default/fcn.ext")
        self.assertEqual(filter_url('http://domain.com/default/fcn/arg'),
                         "/init/default/fcn ['arg']")
        # now raise-HTTP variants
        self.assertRaises(HTTP, filter_url, 'http://domain.com/bad!ctl')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/ctl/bad!fcn')
        self.assertRaises(
            HTTP, filter_url, 'http://domain.com/ctl/fcn.bad!ext')
        self.assertRaises(
            HTTP, filter_url, 'http://domain.com/ctl/fcn/bad!arg')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, '400.*invalid controller', filter_url, 'http://domain.com/init/bad!ctl')
            self.assertRaisesRegexp(HTTP, '400.*invalid function', filter_url,
                                    'http://domain.com/init/ctlr/bad!fcn')
            self.assertRaisesRegexp(HTTP, '400.*invalid extension', filter_url,
                                    'http://domain.com/init/ctlr/fcn.bad!ext')
            self.assertRaisesRegexp(HTTP, '400.*invalid arg', filter_url,
                                    'http://domain.com/appc/init/fcn/bad!arg')
        except AttributeError:
            pass

        self.assertEqual(filter_url('http://domain.com/welcome/default/fcn_1'),
                         "/welcome/default/fcn_1")
        self.assertRaises(
            HTTP, filter_url, 'http://domain.com/welcome/default/fcn-1')
        try:
            # 2.7+ only
            self.assertRaisesRegexp(HTTP, '400.*invalid function', filter_url,
                                    'http://domain.com/welcome/default/fcn-1')
        except AttributeError:
            pass

    def test_router_out(self):
        '''
        Test basic outgoing routing
        '''
        router_out = dict(
            BASE=dict(),
            init=dict(controllers=['default', 'ctr'], ),
            app=dict(),
        )
        load(rdict=router_out)
        self.assertEqual(filter_url(
            'https://domain.com/app/ctr/fcn', out=True), "/app/ctr/fcn")
        self.assertEqual(filter_url(
            'https://domain.com/init/ctr/fcn', out=True), "/ctr/fcn")
        self.assertEqual(filter_url(
            'https://domain.com/init/ctr/fcn', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/init/static/file',
                         out=True), "/init/static/file")
        self.assertEqual(filter_url('https://domain.com/init/static/index',
                         out=True), "/init/static/index")
        self.assertEqual(filter_url(
            'https://domain.com/init/default/index', out=True), "/")
        self.assertEqual(
            filter_url('https://domain.com/init/ctr/index', out=True), "/ctr")
        self.assertEqual(filter_url('http://domain.com/init/default/fcn?query',
                         out=True), "/fcn?query")
        self.assertEqual(filter_url('http://domain.com/init/default/fcn#anchor', out=True), "/fcn#anchor")
        self.assertEqual(
            filter_url(
                'http://domain.com/init/default/fcn?query#anchor', out=True),
            "/fcn?query#anchor")

        router_out['BASE']['map_static'] = True
        load(rdict=router_out)
        self.assertEqual(filter_url(
            'https://domain.com/init/static/file', out=True), "/static/file")
        self.assertEqual(filter_url('https://domain.com/init/static/index',
                         out=True), "/static/index")

        router_out['init']['map_static'] = None
        load(rdict=router_out)
        self.assertEqual(filter_url('https://domain.com/init/static/file',
                         out=True), "/init/static/file")
        self.assertEqual(filter_url('https://domain.com/init/static/index',
                         out=True), "/init/static/index")

    def test_router_functions(self):
        '''
        Test function-omission with functions=[something]
        '''
        router_functions = dict(
            BASE=dict(
                applications=['init', 'app', 'app2'],
                default_application='app',
            ),
            init=dict(
                controllers=['default'],
            ),
            app=dict(
                controllers=['default', 'ctr'],
                functions=dict(
                    default=['index', 'user', 'help'],
                    ctr=['ctrf1', 'ctrf2', 'ctrf3'],
                ),
                default_function=dict(
                    default='index',
                    ctr='ctrf1',
                ),
            ),
            app2=dict(
                controllers=['default', 'ctr'],
                functions=['index', 'user', 'help'],
            ),
        )
        load(rdict=router_functions)

        # outbound
        self.assertEqual(str(
            URL(a='init', c='default', f='f', args=['arg1'])), "/init/f/arg1")
        self.assertEqual(str(URL(a='init', c='default', f='index',
                         args=['arg1'])), "/init/index/arg1")

        self.assertEqual(
            str(URL(a='app', c='default', f='index', args=['arg1'])), "/arg1")
        self.assertEqual(str(
            URL(a='app', c='default', f='user', args=['arg1'])), "/user/arg1")
        self.assertEqual(str(URL(
            a='app', c='default', f='user', args=['index'])), "/user/index")
        self.assertEqual(str(URL(
            a='app', c='default', f='index', args=['index'])), "/index/index")
        self.assertEqual(str(URL(
            a='app', c='default', f='index', args=['init'])), "/index/init")
        self.assertEqual(str(
            URL(a='app', c='default', f='index', args=['ctr'])), "/index/ctr")
        self.assertEqual(str(
            URL(a='app', c='ctr', f='index', args=['arg'])), "/ctr/index/arg")
        self.assertEqual(
            str(URL(a='app', c='ctr', f='ctrf1', args=['arg'])), "/ctr/arg")
        self.assertEqual(str(URL(
            a='app', c='ctr', f='ctrf1', args=['ctrf2'])), "/ctr/ctrf1/ctrf2")

        self.assertEqual(str(URL(
            a='app2', c='default', f='index', args=['arg1'])), "/app2/arg1")
        self.assertEqual(str(URL(a='app2', c='default', f='user',
                         args=['arg1'])), "/app2/user/arg1")
        self.assertEqual(str(URL(a='app2', c='default', f='user',
                         args=['index'])), "/app2/user/index")
        self.assertEqual(str(URL(a='app2', c='default', f='index',
                         args=['index'])), "/app2/index/index")
        self.assertEqual(str(URL(a='app2', c='default', f='index',
                         args=['init'])), "/app2/index/init")
        self.assertEqual(str(URL(a='app2', c='default', f='index',
                         args=['ctr'])), "/app2/index/ctr")

        # inbound
        self.assertEqual(
            filter_url('http://d.com/arg'), "/app/default/index ['arg']")
        self.assertEqual(filter_url('http://d.com/user'), "/app/default/user")
        self.assertEqual(
            filter_url('http://d.com/user/arg'), "/app/default/user ['arg']")
        self.assertEqual(filter_url('http://d.com/ctr'), "/app/ctr/ctrf1")
        self.assertEqual(
            filter_url('http://d.com/ctr/arg'), "/app/ctr/ctrf1 ['arg']")

        self.assertEqual(filter_url(
            'http://d.com/app2/arg'), "/app2/default/index ['arg']")
        self.assertEqual(
            filter_url('http://d.com/app2/user'), "/app2/default/user")
        self.assertEqual(filter_url(
            'http://d.com/app2/user/arg'), "/app2/default/user ['arg']")
        self.assertEqual(
            filter_url('http://d.com/app2/ctr'), "/app2/ctr/index")
        self.assertEqual(filter_url(
            'http://d.com/app2/ctr/index/arg'), "/app2/ctr/index ['arg']")
        self.assertEqual(
            filter_url('http://d.com/app2/ctr/arg'), "/app2/ctr/arg")

    def test_router_functions2(self):
        '''
        Test more functions=[something]
        '''
        router_functions = dict(
            BASE=dict(
                default_application='init',
                applications='INIT',
            ),
            init=dict(
                #default_controller = 'default',
                controllers=['default', 'ctr'],
                #default_function = 'index',
                functions=['index', 'user', 'register', 'basicRegister',
                           'download', 'call', 'data', 'error']
            ),
        )

        load(rdict=router_functions)

        # outbound
        self.assertEqual(str(
            URL(a='init', c='default', f='index', args=['arg1'])), "/arg1")
        self.assertEqual(str(URL(
            a='init', c='default', f='user', args=['arg1'])), "/user/arg1")
        self.assertEqual(str(URL(
            a='init', c='default', f='user', args=['index'])), "/user/index")
        self.assertEqual(str(URL(a='init', c='default', f='index',
                         args=['index'])), "/index/index")
        self.assertEqual(str(
            URL(a='init', c='default', f='index', args=['init'])), "/init")
        self.assertEqual(str(URL(
            a='init', c='default', f='index', args=['ctr'])), "/index/ctr")
        self.assertEqual(str(URL(
            a='init', c='ctr', f='index', args=['arg'])), "/ctr/index/arg")
        self.assertEqual(str(URL(
            a='init', c='ctr', f='ctrf1', args=['arg'])), "/ctr/ctrf1/arg")
        self.assertEqual(str(URL(a='init', c='ctr', f='ctrf1',
                         args=['ctrf2'])), "/ctr/ctrf1/ctrf2")
        self.assertEqual(
            str(URL(a='init', c='default', f='register')), "/register")

        # inbound
        self.assertEqual(
            filter_url('http://d.com/arg'), "/init/default/index ['arg']")
        self.assertEqual(filter_url('http://d.com/user'), "/init/default/user")
        self.assertEqual(
            filter_url('http://d.com/user/arg'), "/init/default/user ['arg']")
        self.assertEqual(filter_url('http://d.com/ctr'), "/init/ctr/index")
        self.assertEqual(filter_url(
            'http://d.com/ctr/ctrf1/arg'), "/init/ctr/ctrf1 ['arg']")

    def test_router_hyphen(self):
        '''
        Test hyphen conversion
        '''
        router_hyphen = dict(
            BASE=dict(
                applications=['init', 'app1', 'app2'],
            ),
            init=dict(
                controllers=['default'],
            ),
            app1=dict(
                controllers=['default'],
                map_hyphen=True,
            ),
            app2=dict(
                controllers=['default'],
                map_hyphen=False,
            ),
        )
        load(rdict=router_hyphen)
        self.assertEqual(filter_url(
            'http://domain.com/init/default/fcn_1', out=True), "/fcn_1")
        self.assertEqual(
            filter_url('http://domain.com/static/filename-with_underscore'),
            norm_root("%s/applications/init/static/filename-with_underscore" % root))
        self.assertEqual(
            filter_url('http://domain.com/init/static/filename-with_underscore', out=True),
            "/init/static/filename-with_underscore")

        self.assertEqual(filter_url('http://domain.com/app2/fcn_1'),
                         "/app2/default/fcn_1")
        self.assertEqual(
            filter_url('http://domain.com/app2/ctr/fcn_1',
                       domain=('app2', None), out=True),
            "/ctr/fcn_1")
        self.assertEqual(
            filter_url('http://domain.com/app2/static/filename-with_underscore', domain=('app2', None), out=True),
            "/app2/static/filename-with_underscore")
        self.assertEqual(
            filter_url(
                'http://domain.com/app2/static/filename-with_underscore'),
            norm_root("%s/applications/app2/static/filename-with_underscore" % root))

        self.assertEqual(str(URL(a='init', c='default', f='a_b')), "/a_b")
        self.assertEqual(str(URL(a='app1', c='default', f='a_b')), "/app1/a-b")
        self.assertEqual(str(URL(a='app2', c='default', f='a_b')), "/app2/a_b")
        self.assertEqual(
            str(URL(a='app1', c='static', f='a/b_c')), "/app1/static/a/b_c")
        self.assertEqual(
            str(URL(a='app1', c='static/a', f='b_c')), "/app1/static/a/b_c")
        self.assertEqual(
            str(URL(a='app2', c='static', f='a/b_c')), "/app2/static/a/b_c")
        self.assertEqual(
            str(URL(a='app2', c='static/a', f='b_c')), "/app2/static/a/b_c")

    def test_router_lang(self):
        '''
        Test language specifications
        '''
        router_lang = dict(
            BASE=dict(default_application='admin'),
            welcome=dict(),
            admin=dict(
                controllers=['default', 'ctr'],
                languages=['en', 'it', 'it-it'], default_language='en',
            ),
            examples=dict(
                languages=['en', 'it', 'it-it'], default_language='en',
            ),
        )
        load(rdict=router_lang)
        self.assertEqual(filter_url('http://domain.com/index/abc'),
                         "/admin/default/index ['abc'] (en)")
        self.assertEqual(filter_url('http://domain.com/en/abc/def'),
                         "/admin/default/abc ['def'] (en)")
        self.assertEqual(filter_url('http://domain.com/it/abc/def'),
                         "/admin/default/abc ['def'] (it)")
        self.assertEqual(filter_url('http://domain.com/it-it/abc/def'),
                         "/admin/default/abc ['def'] (it-it)")
        self.assertEqual(filter_url('http://domain.com/index/a%20bc'),
                         "/admin/default/index ['a bc'] (en)")
        self.assertEqual(filter_url('http://domain.com/static/file'),
                        norm_root("%s/applications/admin/static/file" % root))
        self.assertEqual(filter_url('http://domain.com/en/static/file'),
                        norm_root("%s/applications/admin/static/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/en/static/file'),
                        norm_root("%s/applications/examples/static/en/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/static/file'),
                         norm_root("%s/applications/examples/static/en/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/it/static/file'),
                        norm_root("%s/applications/examples/static/it/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/it-it/static/file'),
                        norm_root("%s/applications/examples/static/file" % root))

        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='en', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it', out=True), "/it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it-it', out=True), "/it-it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='en', out=True), "/admin/en/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', out=True), "/admin/it/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it-it', out=True), "/admin/it-it/static/file")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='it', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='es', out=True), "/welcome/ctr/fcn")

        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         language='en', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         language='it', out=True), "/it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         language='it-it', out=True), "/it-it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         language='en', out=True), "/admin/en/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         language='it', out=True), "/admin/it/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         language='it-it', out=True), "/admin/it-it/static/file")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         language='it', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         language='es', out=True), "/welcome/ctr/fcn")

        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it', language='en', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='en', language='it', out=True), "/it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it', language='it-it', out=True), "/it-it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', language='en', out=True), "/admin/en/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', language='it', out=True), "/admin/it/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', language='it-it', out=True), "/admin/it-it/static/file")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='it', language='it', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='it', language='es', out=True), "/welcome/ctr/fcn")

        router_lang['admin']['map_static'] = True
        load(rdict=router_lang)
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='en', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it', out=True), "/it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it-it', out=True), "/it-it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='en', out=True), "/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', out=True), "/it/static/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it-it', out=True), "/it-it/static/file")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='it', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='es', out=True), "/welcome/ctr/fcn")

        router_lang['admin']['map_static'] = False
        router_lang['examples']['map_static'] = False
        load(rdict=router_lang)
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='en', out=True), "/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it', out=True), "/it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/ctr/fcn',
                         lang='it-it', out=True), "/it-it/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='en', out=True), "/admin/static/en/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it', out=True), "/admin/static/it/file")
        self.assertEqual(filter_url('https://domain.com/admin/static/file',
                         lang='it-it', out=True), "/admin/static/it-it/file")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='it', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('https://domain.com/welcome/ctr/fcn',
                         lang='es', out=True), "/welcome/ctr/fcn")
        self.assertEqual(filter_url('http://domain.com/static/file'),
                         norm_root("%s/applications/admin/static/file" % root))
        self.assertEqual(filter_url('http://domain.com/en/static/file'),
                         norm_root("%s/applications/admin/static/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/en/static/file'),
                        norm_root("%s/applications/examples/static/en/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/static/file'),
                        norm_root("%s/applications/examples/static/en/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/it/static/file'),
                        norm_root("%s/applications/examples/static/it/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/it-it/static/file'),
                        norm_root("%s/applications/examples/static/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/static/en/file').replace('/', os.sep),
                        norm_root("%s/applications/examples/static/en/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/static/it/file').replace('/', os.sep),
                        norm_root("%s/applications/examples/static/it/file" % root))
        self.assertEqual(filter_url('http://domain.com/examples/static/it-it/file').replace('/', os.sep),
                        norm_root("%s/applications/examples/static/it-it/file" % root))

    def test_router_get_effective(self):
        '''
        Test get_effective_router
        '''
        router_get_effective = dict(
            BASE=dict(
                default_application='a1',
                applications=['a1', 'a2'],
            ),
            a1=dict(
                controllers=['c1a', 'c1b', 'default'],
            ),
            a2=dict(
                default_controller='c2',
                controllers=[],
            ),
            a3=dict(
                default_controller='c2',
                controllers=['c1'],
            ),
            a4=dict(
                default_function='f1',
                functions=['f2'],
            ),
        )
        load(rdict=router_get_effective)
        self.assertEqual(
            get_effective_router('BASE').applications, set(['a1', 'a2']))
        self.assertEqual(
            get_effective_router('BASE').default_application, 'a1')
        self.assertEqual(get_effective_router('BASE').domains, {})
        self.assertEqual(get_effective_router('a1').applications, None)
        self.assertEqual(get_effective_router('a1').default_application, None)
        self.assertEqual(get_effective_router('a1').domains, None)
        self.assertEqual(
            get_effective_router('a1').default_controller, "default")
        self.assertEqual(get_effective_router('a2').default_application, None)
        self.assertEqual(get_effective_router('a2').default_controller, "c2")
        self.assertEqual(get_effective_router(
            'a1').controllers, set(['c1a', 'c1b', 'default', 'static']))
        self.assertEqual(get_effective_router('a2').controllers, set())
        self.assertEqual(get_effective_router(
            'a3').controllers, set(['c1', 'c2', 'static']))
        self.assertEqual(get_effective_router(
            'a4').functions, dict(default=set(['f1', 'f2'])))
        self.assertEqual(get_effective_router('xx'), None)

    def test_router_error(self):
        '''
        Test rewrite of HTTP errors
        '''
        router_err = dict()
        load(rdict=router_err)
        self.assertEqual(filter_err(200), 200)
        self.assertEqual(filter_err(399), 399)
        self.assertEqual(filter_err(400), 400)

    def test_router_static_path(self):
        '''
        Test validation of static paths
        Stock pattern: file_match = r'([-+=@$%\w]+[./]?)+$'

        '''
        load(rdict=dict())
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/st~tic')
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to--/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to--/static" % root))
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/==to--/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/==to--/static" % root))
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/-+=@$%/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/-+=@$%%/static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/.static')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/s..tatic')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to//static')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/#static')

        router_static = dict(
            BASE=dict(
                file_match=r'([-+=@$%#\w]+[./]?)+$',   # legal static path
            ),
        )
        load(rdict=router_static)
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/#static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/#static" % root))

        router_static = dict(
            BASE=dict(
                file_match=r'[-+=@$%#.\w]+$',   # legal static path element
            ),
        )
        load(rdict=router_static)
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to/st~tic')
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to--/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to--/static" % root))
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/==to--/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/==to--/static" % root))
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/-+=@$%/static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/-+=@$%%/static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/to//static')
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/#static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/#static" % root))
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/./static')
        self.assertRaises(HTTP, filter_url, 'http://domain.com/welcome/static/bad/path/../static')
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/.../static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/.../static" % root))
        self.assertEqual(filter_url('http://domain.com/welcome/static/path/to/.static').replace('/', os.sep),
            norm_root("%s/applications/welcome/static/path/to/.static" % root))

    def test_router_args(self):
        '''
        Test URL args parsing/generation
        '''
        load(rdict=dict())
        self.assertEqual(filter_url('http://domain.com/init/default/f/arg1'),
                         "/init/default/f ['arg1']")
        self.assertEqual(filter_url('http://domain.com/init/default/f/arg1/'),
                         "/init/default/f ['arg1']")
        self.assertEqual(filter_url('http://domain.com/init/default/f/arg1//'),
                         "/init/default/f ['arg1', '']")
        self.assertEqual(filter_url('http://domain.com/init/default/f//arg1'),
                         "/init/default/f ['', 'arg1']")
        self.assertEqual(
            filter_url('http://domain.com/init/default/f/arg1/arg2'),
            "/init/default/f ['arg1', 'arg2']")
        self.assertEqual(
            filter_url('http://domain.com/init/default/f/arg1//arg2'),
            "/init/default/f ['arg1', '', 'arg2']")
        self.assertEqual(
            filter_url('http://domain.com/init/default/f/arg1//arg3/'),
            "/init/default/f ['arg1', '', 'arg3']")
        self.assertEqual(
            filter_url('http://domain.com/init/default/f/arg1//arg3//'),
            "/init/default/f ['arg1', '', 'arg3', '']")

        self.assertEqual(
            filter_url('http://domain.com/init/default/f', out=True), "/f")
        self.assertEqual(map_url_out(None, None, 'init', 'default',
                         'f', None, None, None, None, None), "/f")
        self.assertEqual(map_url_out(None, None, 'init', 'default',
                         'f', [], None, None, None, None), "/f")
        self.assertEqual(map_url_out(None, None, 'init', 'default',
                         'f', ['arg1'], None, None, None, None), "/f")
        self.assertEqual(map_url_out(None, None, 'init', 'default',
                         'f', ['arg1', ''], None, None, None, None), "/f")
        self.assertEqual(
            str(URL(a='init', c='default', f='f', args=None)), "/f")
        self.assertEqual(
            str(URL(a='init', c='default', f='f', args=['arg1'])), "/f/arg1")
        self.assertEqual(str(URL(
            a='init', c='default', f='f', args=['arg1', ''])), "/f/arg1//")
        self.assertEqual(str(URL(a='init', c='default', f='f',
                         args=['arg1', '', 'arg3'])), "/f/arg1//arg3")
        self.assertEqual(str(
            URL(a='init', c='default', f='f', args=['ar g'])), "/f/ar%20g")
        self.assertEqual(str(
            URL(a='init', c='default', f='f', args=['årg'])), "/f/%C3%A5rg")
        self.assertEqual(
            str(URL(a='init', c='default', f='fünc')), "/f\xc3\xbcnc")

    def test_routes_anchor(self):
        '''
        Test URL with anchor
        '''
        self.assertEqual(
            str(URL(a='a', c='c', f='f', anchor='anchor')), "/a/c/f#anchor")
        load(rdict=dict())
        self.assertEqual(
            str(URL(a='a', c='c', f='f', anchor='anchor')), "/a/c/f#anchor")
        args = ['a1', 'a2']
        self.assertEqual(
            str(URL(a='a', c='c', f='f', args=args, anchor='anchor')),
            "/a/c/f/a1/a2#anchor")
        vars = dict(v1=1, v2=2)
        self.assertEqual(
            str(URL(a='a', c='c', f='f', vars=vars, anchor='anchor')),
            "/a/c/f?v1=1&v2=2#anchor")
        self.assertEqual(
            str(URL(
                a='a', c='c', f='f', args=args, vars=vars, anchor='anchor')),
            "/a/c/f/a1/a2?v1=1&v2=2#anchor")
        self.assertEqual(str(URL(a='init', c='default', f='index')),
                         "/")
        self.assertEqual(str(URL(a='init', c='default', f='f')),
                         "/f")
        self.assertEqual(
            str(URL(a='init', c='default', f='index', anchor='anchor')),
            "/#anchor")
        self.assertEqual(
            str(URL(a='init', c='default', f='f', anchor='anchor')),
            "/f#anchor")

    def test_router_prefix(self):
        '''
        Test path_prefix
        '''
        router_path_prefix = dict(
            BASE=dict(
                default_application='a1',
                applications=['a1', 'a2'],
                path_prefix='/path/to/apps',
            ),
            a1=dict(
                controllers=['c1a', 'c1b', 'default'],
            ),
            a2=dict(
                default_controller='c2',
                controllers=[],
            ),
        )
        load(rdict=router_path_prefix)
        self.assertEqual(str(URL(a='a1', c='c1a', f='f')),
                         "/path/to/apps/c1a/f")
        self.assertEqual(str(URL(a='a2', c='c', f='f')),
                         "/path/to/apps/a2/c/f")
        self.assertEqual(str(URL(a='a2', c='c2', f='f')),
                         "/path/to/apps/a2/c2/f")
        self.assertEqual(
            filter_url('http://domain.com/a1/'), "/a1/default/index")
        self.assertEqual(filter_url(
            'http://domain.com/path/to/apps/a1/'), "/a1/default/index")
        self.assertEqual(filter_url(
            'http://domain.com/path/to/a1/'), "/a1/default/path ['to', 'a1']")

    def test_router_absolute(self):
        '''
        Test absolute URL
        '''
        load(rdict=dict())
        r = Storage()
        r.env = Storage()
        r.env.http_host = 'domain.com'
        r.env.wsgi_url_scheme = 'httpx'  # distinguish incoming scheme
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f')), "/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', host=True)),
                         "httpx://domain.com/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', host='host.com')),
                         "httpx://host.com/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', scheme=True)),
                         "httpx://domain.com/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', scheme=False)),
                         "/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', scheme='https')),
                         "https://domain.com/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', scheme='wss')),
                         "wss://domain.com/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme=True, host=True)),
            "httpx://domain.com/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme='https', host=True)),
            "https://domain.com/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme=False, host=True)),
            "httpx://domain.com/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme=True, host='host.com')),
            "httpx://host.com/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme=False, host='host.com')),
            "httpx://host.com/a/c/f")
        self.assertEqual(str(URL(r=r, a='a', c='c', f='f', port=1234)),
                         "httpx://domain.com:1234/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme=True, port=1234)),
            "httpx://domain.com:1234/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', host='host.com', port=1234)),
            "httpx://host.com:1234/a/c/f")
        self.assertEqual(
            str(URL(r=r, a='a', c='c', f='f', scheme='wss',
                host='host.com', port=1234)),
            "wss://host.com:1234/a/c/f")

    def test_request_uri(self):
        '''
        Test REQUEST_URI in env
        '''
        load(rdict=dict())

        self.assertEqual(
            filter_url('http://domain.com/abc', env=True).request_uri,
            '/init/default/abc')
        self.assertEqual(
            filter_url('http://domain.com/abc?def', env=True).request_uri,
            '/init/default/abc?def')
        self.assertEqual(
            filter_url('http://domain.com/index/abc', env=True).request_uri,
            "/init/default/index/abc")
        self.assertEqual(
            filter_url('http://domain.com/abc/def', env=True).request_uri,
            "/init/default/abc/def")
        self.assertEqual(
            filter_url('http://domain.com/index/a%20bc', env=True).request_uri,
            "/init/default/index/a%20bc")

    def test_request_collide(self):
        '''
        Test controller-app name collision: admin vs welcome/admin
        '''
        router_collide = dict(
            BASE=dict(
                domains={
                    'ex.domain.com': 'examples',
                    'ad.domain.com': 'admin',
                    'welcome.com': 'welcome',
                    'www.welcome.com': 'welcome',
                },
                exclusive_domain=True,
            ),
        )
        load(rdict=router_collide)

        # basic inbound
        self.assertEqual(
            filter_url('http://ex.domain.com'), '/examples/default/exdef')
        self.assertEqual(
            filter_url('http://ad.domain.com'), '/admin/default/index')
        self.assertEqual(
            filter_url('http://welcome.com'), '/welcome/default/index')
        self.assertEqual(
            filter_url('http://www.welcome.com'), '/welcome/default/index')
        # basic outbound
        self.assertEqual(filter_url('http://ex.domain.com/examples/default/exdef', domain='examples', out=True), "/")
        self.assertEqual(filter_url('http://ad.domain.com/admin/default/index',
                         domain='admin', out=True), "/")
        self.assertEqual(filter_url('http://welcome.com/welcome/default/index',
                         domain='welcome', out=True), "/")
        self.assertEqual(filter_url('http://www.welcome.com/welcome/default/index', domain='welcome', out=True), "/")

        # inbound
        self.assertEqual(
            filter_url('http://welcome.com/admin'), '/welcome/admin/index')
        self.assertEqual(
            filter_url('http://welcome.com/f1'), '/welcome/default/f1')
        self.assertEqual(
            filter_url('http://ad.domain.com/shell'), '/admin/shell/index')
        self.assertEqual(
            filter_url('http://ad.domain.com/f1'), '/admin/default/f1')
        # outbound
        self.assertEqual(filter_url('http://welcome.com/welcome/other/index',
                         domain='welcome', out=True), "/other")
        self.assertEqual(filter_url('http://welcome.com/welcome/admin/index',
                         domain='welcome', out=True), "/admin")
        self.assertEqual(filter_url('http://ad.domain.com/admin/shell/index',
                         domain='admin', out=True), "/shell")
        self.assertEqual(filter_url('http://ad.domain.com/admin/default/f1',
                         domain='admin', out=True), "/f1")
        router_collide['BASE']['exclusive_domain'] = False
        load(rdict=router_collide)
        self.assertEqual(filter_url('http://welcome.com/welcome/admin/index',
                         domain='welcome', out=True), "/welcome/admin")


if __name__ == '__main__':
    setUpModule()       # pre-2.7
    unittest.main()
    tearDownModule()
