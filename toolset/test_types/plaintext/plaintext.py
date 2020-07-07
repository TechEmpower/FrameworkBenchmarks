from toolset.test_types.verifications import basic_body_verification, verify_headers
from toolset.test_types.abstract_test_type import AbstractTestType


class TestType(AbstractTestType):
    def __init__(self, config):
        self.plaintext_url = ""
        kwargs = {
            'name': 'plaintext',
            'requires_db': False,
            'accept_header': self.accept('plaintext'),
            'args': ['plaintext_url']
        }
        AbstractTestType.__init__(self, config, **kwargs)

    def verify(self, base_url):
        url = base_url + self.plaintext_url
        headers, body = self.request_headers_and_body(url)

        _, problems = basic_body_verification(body, url, is_json_check=False)

        # plaintext_url should be at least "/plaintext"
        if len(self.plaintext_url) < 10:
            problems.append(
                ("fail",
                 "Route for plaintext must be at least 10 characters, found '{}' instead".format(self.plaintext_url),
                 url))

        if len(problems) > 0:
            return problems

        # Case insensitive
        body = body.lower()
        expected = "hello, world!"
        extra_bytes = len(body) - len(expected)

        if expected not in body:
            return [('fail', "Could not find 'Hello, World!' in response.",
                     url)]

        if extra_bytes > 0:
            problems.append(
                ('warn',
                 ("Server is returning %s more bytes than are required. "
                  "This may negatively affect benchmark performance." %
                  extra_bytes), url))

        problems += verify_headers(self.request_headers_and_body, headers, url, should_be='plaintext')

        if len(problems) == 0:
            return [('pass', '', url)]
        else:
            return problems

    def get_url(self):
        return self.plaintext_url

    def get_script_name(self):
        return 'pipeline.sh'

    def get_script_variables(self, name, url):
        return {
            'max_concurrency':
                max(self.config.concurrency_levels),
            'name':
                name,
            'duration':
                self.config.duration,
            'levels':
                " ".join("{}".format(item)
                         for item in self.config.pipeline_concurrency_levels),
            'server_host':
                self.config.server_host,
            'url':
                url,
            'pipeline':
                16,
            'accept':
                "text/plain,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
        }
