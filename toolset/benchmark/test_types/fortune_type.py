from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.fortune_html_parser import FortuneHTMLParser


class FortuneTestType(FrameworkTestType):

    def __init__(self):
        kwargs = {
            'name': 'fortune',
            'accept_header': self.accept_html,
            'requires_db': True,
            'args': ['fortune_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.fortune_url

    def verify(self, base_url):
        '''Parses the given HTML string and asks the 
        FortuneHTMLParser whether the parsed string is a 
        valid fortune response
        '''

        url = base_url + self.fortune_url
        headers, body = self.request_headers_and_body(url)

        # Empty response
        if body is None:
            return [('fail', 'No response', url)]
        elif len(body) == 0:
            return [('fail', 'Empty Response', url)]

        parser = FortuneHTMLParser()
        parser.feed(body)
        (valid, diff) = parser.isValidFortune(self.out)

        if valid:
            problems = []
            problems += self._verifyHeaders(headers, url)

            if len(problems) == 0:
                return [('pass', '', url)]
            else:
                return problems
        else:
            failures = []
            failures.append(
                ('fail', 'Invalid according to FortuneHTMLParser', url))
            failures += self._parseDiffForFailure(diff, failures, url)
            return failures

    def _parseDiffForFailure(self, diff, failures, url):
        '''Example diff:

        --- Valid
        +++ Response
        @@ -1 +1 @@

        -<!doctype html><html><head><title>Fortunes</title></head><body><table>
        +<!doctype html><html><head><meta></meta><title>Fortunes</title></head><body><div><table>
        @@ -16 +16 @@
        '''

        problems = []

        # Catch exceptions because we are relying on internal code
        try:
            current_neg = []
            current_pos = []
            for line in diff[3:]:
                if line[0] == '+':
                    current_neg.append(line[1:])
                elif line[0] == '-':
                    current_pos.append(line[1:])
                elif line[0] == '@':
                    problems.append(('fail',
                                     "`%s` should be `%s`" % (
                                         ''.join(current_neg), ''.join(current_pos)),
                                     url))
            if len(current_pos) != 0:
                problems.append(
                    ('fail',
                     "`%s` should be `%s`" % (
                         ''.join(current_neg), ''.join(current_pos)),
                     url))
        except:
            # If there were errors reading the diff, then no diff information
            pass
        return problems

    def _verifyHeaders(self, headers, url):
        '''Verifies the response headers for the Fortunes test'''

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
            expected_type = 'text/html'
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
