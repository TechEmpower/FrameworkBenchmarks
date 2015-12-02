from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.verifications import basic_body_verification, verify_headers


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

        _, problems = basic_body_verification(body, url, is_json_check=False)

        if len(problems) > 0:
            return problems

        # Case insensitive
        orig = body
        body = body.lower()
        expected = "hello, world!"
        extra_bytes = len(body) - len(expected)

        if expected not in body:
            return [('fail', "Could not find 'Hello, World!' in response.", url)]

        if extra_bytes > 0:
            problems.append(
                ('warn',
                 ("Server is returning %s more bytes than are required. "
                  "This may negatively affect benchmark performance."
                  % (extra_bytes)),
                 url))

        problems += verify_headers(headers, url, should_be='plaintext')

        if len(problems) == 0:
            return [('pass', '', url)]
        else:
            return problems

    def get_url(self):
        return self.plaintext_url
