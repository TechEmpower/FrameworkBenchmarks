from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.verifications import (
    verify_headers,
    verify_randomnumber_list,
    verify_query_cases
)


class CachedQueryTestType(FrameworkTestType):

    def __init__(self):
        kwargs = {
            'name': 'cached_query',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['cached_query_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.cached_query_url

    def verify(self, base_url):
        '''
        Validates the response is a JSON array of
        the proper length, each JSON Object in the array
        has keys 'id' and 'randomNumber', and these keys
        map to integers. Case insensitive and
        quoting style is ignored
        '''

        url = base_url + self.cached_query_url
        cases = [
            ('2',   'fail'),
            ('0',   'fail'),
            ('foo', 'fail'),
            ('501', 'warn'),
            ('',    'fail')
        ]

        problems = verify_query_cases(self, cases, url)

        if len(problems) == 0:
            return [('pass', '', url + case) for case, _ in cases]
        else:
            return problems
