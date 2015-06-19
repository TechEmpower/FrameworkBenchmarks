from benchmark.test_types.framework_test_type import FrameworkTestType

import json


class JsonTestType(FrameworkTestType):

    def __init__(self):
        kwargs = {
            'name': 'json',
            'accept_header': self.accept('json'),
            'requires_db': False,
            'args': ['json_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.json_url

    def verify(self, base_url):
        '''Validates the response is a JSON object of 
        { 'message' : 'hello, world!' }. Case insensitive and 
        quoting style is ignored
        '''

        url = base_url + self.json_url
        headers, body = self.request_headers_and_body(url)

        # Empty response
        if body is None:
            return [('fail', 'No response', url)]
        elif len(body) == 0:
            return [('fail', 'Empty Response', url)]

        # Valid JSON?
        try:
            response = json.loads(body)
        except ValueError as ve:
            return [('fail', "Invalid JSON - %s" % ve, url)]

        problems = []
        problems += self._verifyObject(response, url)
        problems += self._verifyHeaders(headers, url)

        if len(problems) > 0:
            return problems
        else:
            return [('pass', '', url)]

    def _verifyObject(self, json_object, url):
        '''
        Ensure that the JSON object closely resembles
        { 'message': 'Hello, World!' }
        '''

        problems = []

        # Make everything case insensitive
        json_object = {k.lower(): v.lower()
                       for k, v in json_object.iteritems()}

        if 'message' not in json_object:
            return [('fail', "Missing required key 'message'", url)]
        else:
            if len(json_object) != 1:
                additional = (', ').join(
                    [k for k in json_object.keys() if k != 'message'])
                problems.append(
                    ('warn', "Too many JSON key/value pairs, consider removing: %s" % additional, url))

            message = json_object['message']
            if message != 'hello, world!':
                return [('fail', "Expected message of 'hello, world!', got '%s'" % message)]

        return problems

    def _verifyHeaders(self, headers, url):
        '''Verifies the response headers'''

        problems = []

        if any(v.lower() not in headers for v in ('Server', 'Date', 'Content-Type')):
            problems.append(
                ('warn', 'Required response header missing: %s' % v, url))
        elif all(v.lower() not in headers for v in ('Content-Length', 'Transfer-Encoding')):
            problems.append(
                ('warn',
                 'Required response size header missing, please include either "Content-Length" or "Transfer-Encoding"',
                 url))
        else:
            content_type = headers.get('Content-Type', None)
            expected_type = 'application/json'
            includes_charset = 'application/json; charset=utf-8'
            if content_type == includes_charset:
                problems.append(
                    ('warn',
                     ("Content encoding \"%s\" found where \"%s\" is acceptable.\n"
                      "Additional response bytes may negatively affect benchmark performance."
                      % (includes_charset, expected_type)),
                     url))
            elif content_type != expected_type:
                problems.append(
                    ('warn',
                     'Unexpected content encoding, found %s, expected %s' % (
                         content_type, expected_type),
                     url))
        return problems
