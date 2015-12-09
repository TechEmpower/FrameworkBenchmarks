from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.verifications import (
    basic_body_verification,
    verify_headers,
    verify_helloworld_object
)

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

        response, problems = basic_body_verification(body, url)

        if len(problems) > 0:
            return problems

        problems += verify_helloworld_object(response, url)
        problems += verify_headers(headers, url, should_be='json')

        if len(problems) > 0:
            return problems
        else:
            return [('pass', '', url)]
