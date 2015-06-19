from benchmark.test_types.framework_test_type import FrameworkTestType


class PlaintextTestType(FrameworkTestType):

    def __init__(self):
        kwargs = {
            'name': 'plaintext',
            'requires_db': False,
            'accept_header': self.accept('plaintext'),
            'args': ['plaintext_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def verify(self, base_url):
        url = base_url + self.plaintext_url
        headers, body = self.request_headers_and_body(url)

        # Empty response
        if body is None:
            return [('fail', 'No response', url)]
        elif len(body) == 0:
            return [('fail', 'Empty Response', url)]

        # Case insensitive
        orig = body
        body = body.lower()
        expected = "hello, world!"
        extra_bytes = len(body) - len(expected)

        if expected not in body:
            return [('fail', "Could not find 'Hello, World!' in response.", url)]

        problems = []

        if extra_bytes > 0:
            problems.append(
                ('warn',
                 ("Server is returning %s more bytes than are required. "
                  "This may negatively affect benchmark performance."
                  % (extra_bytes)),
                 url))

        problems += self._verifyHeaders(headers, url)

        if len(problems) == 0:
            return [('pass', '', url)]
        else:
            return problems

    def get_url(self):
        return self.plaintext_url

    def _verifyHeaders(self, headers, url):
        '''Verifies the response headers for the Plaintext test'''

        problems = []

        if any(v.lower() not in headers for v in ('Server', 'Date', 'Content-Type')):
            problems.append(
                ('warn', 'Required response header missing: %s' % v, url))
        elif all(v.lower() not in headers for v in ('Content-Length', 'Transfer-Encoding')):
            problems.append(
                ('warn',
                 ('Required response size header missing, '
                  'please include either "Content-Length" or "Transfer-Encoding"'),
                 url))
        else:
            content_type = headers.get('Content-Type', '')
            expected_type = 'text/plain'
            includes_charset = expected_type + '; charset=utf-8'

            if content_type.lower() == includes_charset:
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
