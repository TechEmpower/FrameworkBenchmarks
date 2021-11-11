from toolset.test_types.abstract_test_type import AbstractTestType
from toolset.test_types.verifications import basic_body_verification, verify_headers, verify_helloworld_object

class TestType(AbstractTestType):
    def __init__(self, config):
        self.json_url = ""
        kwargs = {
            'name': 'json',
            'accept_header': self.accept('json'),
            'requires_db': False,
            'args': ['json_url']
        }
        AbstractTestType.__init__(self, config, **kwargs)

    def get_url(self):
        return self.json_url

    def verify(self, base_url):
        '''
        Validates the response is a JSON object of
        { 'message' : 'hello, world!' }. Case insensitive and
        quoting style is ignored
        '''

        url = base_url + self.json_url
        headers, body = self.request_headers_and_body(url)

        response, problems = basic_body_verification(body, url)

        # json_url should be at least "/json"
        if len(self.json_url) < 5:
            problems.append(
                ("fail",
                 "Route for json must be at least 5 characters, found '{}' instead".format(self.json_url),
                 url))

        if len(problems) > 0:
            return problems

        problems += verify_helloworld_object(response, url)
        problems += verify_headers(self.request_headers_and_body, headers, url, should_be='json')

        if len(problems) > 0:
            return problems
        else:
            return [('pass', '', url)]

    def get_script_name(self):
        return 'concurrency.sh'

    def get_script_variables(self, name, url):
        return {
            'max_concurrency':
            max(self.config.concurrency_levels),
            'name':
            name,
            'duration':
            self.config.duration,
            'levels':
            " ".join(
                "{}".format(item) for item in self.config.concurrency_levels),
            'server_host':
            self.config.server_host,
            'url':
            url,
            'accept':
            "application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
        }
