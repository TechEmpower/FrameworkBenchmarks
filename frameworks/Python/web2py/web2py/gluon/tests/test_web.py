#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Unit tests for running web2py
"""
import sys
import os
if sys.version_info < (2, 7):
    import unittest2 as unittest
else:
    import unittest
import subprocess
import time
import signal

from fix_path import fix_sys_path

fix_sys_path(__file__)

from contrib.webclient import WebClient
from urllib2 import HTTPError

webserverprocess = None

def startwebserver():
    global webserverprocess
    path = path = os.path.dirname(os.path.abspath(__file__))
    if not os.path.isfile(os.path.join(path,'web2py.py')):
        i = 0
        while i<10:
            i += 1
            if os.path.exists(os.path.join(path,'web2py.py')):
                break
            path = os.path.abspath(os.path.join(path, '..'))
    web2py_exec = os.path.join(path, 'web2py.py')
    webserverprocess = subprocess.Popen([sys.executable, web2py_exec, '-a',  'testpass'])
    print 'Sleeping before web2py starts...'
    for a in range(1,11):
        time.sleep(1)
        print a, '...'
        try:
            c = WebClient('http://127.0.0.1:8000')
            c.get('/')
            break
        except:
            continue
    print ''

def terminate_process(pid):
    #Taken from http://stackoverflow.com/questions/1064335/in-python-2-5-how-do-i-kill-a-subprocess
    # all this **blah** is because we are stuck with Python 2.5 and \
    #we cannot use Popen.terminate()
    if sys.platform.startswith('win'):
        import ctypes
        PROCESS_TERMINATE = 1
        handle = ctypes.windll.kernel32.OpenProcess(PROCESS_TERMINATE, False, pid)
        ctypes.windll.kernel32.TerminateProcess(handle, -1)
        ctypes.windll.kernel32.CloseHandle(handle)
    else:
        os.kill(pid, signal.SIGKILL)

def stopwebserver():
    global webserverprocess
    print 'Killing webserver'
    if sys.version_info < (2,6):
        terminate_process(webserverprocess.pid)
    else:
        webserverprocess.terminate()


class LiveTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        startwebserver()

    @classmethod
    def tearDownClass(cls):
        stopwebserver()


@unittest.skipIf("datastore" in os.getenv("DB", ""), "TODO: setup web test for app engine")
class TestWeb(LiveTest):
    def testRegisterAndLogin(self):
        client = WebClient('http://127.0.0.1:8000/welcome/default/')

        client.get('index')

        # register
        data = dict(first_name='Homer',
                    last_name='Simpson',
                    email='homer@web2py.com',
                    password='test',
                    password_two='test',
                    _formname='register')
        client.post('user/register', data=data)

        # logout
        client.get('user/logout')

        # login again
        data = dict(email='homer@web2py.com',
                    password='test',
                    _formname='login')
        client.post('user/login', data=data)
        self.assertTrue('Welcome Homer' in client.text)

        # check registration and login were successful
        client.get('index')

        # COMMENTED BECAUSE FAILS BUT WHY?
        self.assertTrue('Welcome Homer' in client.text)

        client = WebClient('http://127.0.0.1:8000/admin/default/')
        client.post('index', data=dict(password='hello'))
        client.get('site')
        client.get('design/welcome')

    def testStaticCache(self):
        s = WebClient('http://127.0.0.1:8000/welcome/')
        s.get('static/js/web2py.js')
        assert('expires' not in s.headers)
        assert(not s.headers['cache-control'].startswith('max-age'))
        text = s.text
        s.get('static/_1.2.3/js/web2py.js')
        assert(text == s.text)
        assert('expires' in s.headers)
        assert(s.headers['cache-control'].startswith('max-age'))

    def testSoap(self):
        # test soap server implementation
        from gluon.contrib.pysimplesoap.client import SoapClient, SoapFault
        url = 'http://127.0.0.1:8000/examples/soap_examples/call/soap?WSDL'
        client = SoapClient(wsdl=url)
        ret = client.SubIntegers(a=3, b=2)
        # check that the value returned is ok
        assert('SubResult' in ret)
        assert(ret['SubResult'] == 1)

        try:
            ret = client.Division(a=3, b=0)
        except SoapFault, sf:
            # verify the exception value is ok
            # assert(sf.faultstring == "float division by zero") # true only in 2.7
            assert(sf.faultcode == "Server.ZeroDivisionError")

        # store sent and received xml for low level test
        xml_request = client.xml_request
        xml_response = client.xml_response

        # do a low level raw soap request (using
        s = WebClient('http://127.0.0.1:8000/')
        try:
            s.post('examples/soap_examples/call/soap', data=xml_request, method="POST")
        except HTTPError, e:
            assert(e.msg=='INTERNAL SERVER ERROR')
        # check internal server error returned (issue 153)
        assert(s.status == 500)
        assert(s.text == xml_response)


if __name__ == '__main__':
    unittest.main()
