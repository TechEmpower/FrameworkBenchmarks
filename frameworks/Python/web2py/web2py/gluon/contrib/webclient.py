"""
Developed by Massimo Di Pierro
Released under the web2py license (LGPL)

It an interface on top of urllib2 which simplifies scripting of http requests
mostly for testing purposes

- customizable
- supports basic auth
- supports cookies
- supports session cookies (tested with web2py sessions)
- detects broken session
- detects web2py form postbacks and handles formname and formkey
- detects web2py tickets

Some examples at the bottom.
"""
import re
import time
import urllib
import urllib2
import cookielib


DEFAULT_HEADERS = {
    'user-agent': 'Mozilla/4.0',  # some servers are picky
    'accept-language': 'en',
}

FORM_REGEX = re.compile('(\<input name\="_formkey" type\="hidden" value\="(?P<formkey>.+?)" \/\>)?\<input name\="_formname" type\="hidden" value\="(?P<formname>.+?)" \/\>')

SESSION_REGEX = 'session_id_(?P<name>.+)'


class WebClient(object):

    def __init__(self,
                 app='',
                 postbacks=True,
                 default_headers=DEFAULT_HEADERS,
                 session_regex=SESSION_REGEX):
        self.app = app
        self.postbacks = postbacks
        self.forms = {}
        self.history = []
        self.cookies = {}
        self.default_headers = default_headers
        self.sessions = {}
        self.session_regex = session_regex and re.compile(session_regex)

    def get(self, url, cookies=None, headers=None, auth=None):
        return self.post(url, data=None, cookies=cookies,
                         headers=headers, method='GET')

    def post(self, url, data=None, cookies=None,
             headers=None, auth=None, method='auto'):
        self.url = self.app + url

        # if this POST form requires a postback do it
        if data and '_formname' in data and self.postbacks and \
                self.history and self.history[-1][1] != self.url:
            # to bypass the web2py CSRF need to get formkey
            # before submitting the form
            self.get(url, cookies=cookies, headers=headers, auth=auth)

        # unless cookies are specified, recycle cookies
        if cookies is None:
            cookies = self.cookies
        cookies = cookies or {}
        headers = headers or {}

        cj = cookielib.CookieJar()
        args = [
            urllib2.HTTPCookieProcessor(cj),
            urllib2.HTTPHandler(debuglevel=0)
            ]
        # if required do basic auth
        if auth:
            auth_handler = urllib2.HTTPBasicAuthHandler()
            auth_handler.add_password(**auth)
            args.append(auth_handler)

        opener = urllib2.build_opener(*args)

        # copy headers from dict to list of key,value
        headers_list = []
        for key, value in self.default_headers.iteritems():
            if not key in headers:
                headers[key] = value
        for key, value in headers.iteritems():
            if isinstance(value, (list, tuple)):
                for v in value:
                    headers_list.append((key, v))
            else:
                headers_list.append((key, value))

        # move cookies to headers
        for key, value in cookies.iteritems():
            headers_list.append(('Cookie', '%s=%s' % (key, value)))

        # add headers to request
        for key, value in headers_list:
            opener.addheaders.append((key, str(value)))

        # assume everything is ok and make http request
        error = None
        try:
            if isinstance(data, str):
                self.method = 'POST' if method=='auto' else method
            elif isinstance(data, dict):
                self.method = 'POST' if method=='auto' else method
                # if there is only one form, set _formname automatically
                if not '_formname' in data and len(self.forms) == 1:
                    data['_formname'] = self.forms.keys()[0]

                # if there is no formkey but it is known, set it
                if '_formname' in data and not '_formkey' in data and \
                        data['_formname'] in self.forms:
                    data['_formkey'] = self.forms[data['_formname']]

                # time the POST request
                data = urllib.urlencode(data, doseq=True)
            else:
                self.method = 'GET' if method=='auto' else method
                data = None
            t0 = time.time()
            self.response = opener.open(self.url, data)
            self.time = time.time() - t0
        except urllib2.HTTPError, error:
            # catch HTTP errors
            self.time = time.time() - t0
            self.response = error

        if hasattr(self.response, 'getcode'):
            self.status = self.response.getcode()
        else:#python2.5
            self.status = None

        self.text = self.response.read()
        self.headers = dict(self.response.headers)

        # treat web2py tickets as special types of errors
        if error is not None:
            if 'web2py_error' in self.headers:
                raise RuntimeError(self.headers['web2py_error'])
            else:
                raise error

        # parse headers into cookies
        self.cookies = {}
        if 'set-cookie' in self.headers:
            for item in self.headers['set-cookie'].split(','):
                key, value = item[:item.find(';')].split('=')
            self.cookies[key.strip()] = value.strip()

        # check is a new session id has been issued, symptom of broken session
        if self.session_regex is not None:
            for cookie, value in self.cookies.iteritems():
                match = self.session_regex.match(cookie)
                if match:
                    name = match.group('name')
                    if name in self.sessions and self.sessions[name] != value:
                        print RuntimeError('Changed session ID %s' % name)
                    self.sessions[name] = value

        # find all forms and formkeys in page
        self.forms = {}
        for match in FORM_REGEX.finditer(self.text):
            self.forms[match.group('formname')] = match.group('formkey')

        # log this request
        self.history.append((self.method, self.url, self.status, self.time))


def test_web2py_registration_and_login():
    # from gluon.contrib.webclient import WebClient
    # start a web2py instance for testing

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

    # login
    data = dict(email='homer@web2py.com',
                password='test',
                _formname='login')
    client.post('user/login', data=data)

    # check registration and login were successful
    client.get('user/profile')
    assert 'Welcome Homer' in client.text

    # print some variables
    print '\nsessions:\n', client.sessions
    print '\nheaders:\n', client.headers
    print '\ncookies:\n', client.cookies
    print '\nforms:\n', client.forms
    print
    for method, url, status, t in client.history:
        print method, url, status, t

if __name__ == '__main__':
    test_web2py_registration_and_login()
