from toolset.benchmark.test_types.framework_test_type import FrameworkTestType
from toolset.benchmark.test_types.verifications import verify_query_cases


class QueryTestType(FrameworkTestType):
    def __init__(self):
        self.query_url = ""
        kwargs = {
            'name': 'query',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['query_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.query_url

    def verify(self, base_url):
        '''
        Validates the response is a JSON array of 
        the proper length, each JSON Object in the array 
        has keys 'id' and 'randomNumber', and these keys 
        map to integers. Case insensitive and 
        quoting style is ignored
        '''

        url = base_url + self.query_url
        cases = [('2', 'fail'), ('0', 'fail'), ('foo', 'fail'),
                 ('501', 'warn'), ('', 'fail')]

        problems = verify_query_cases(self, cases, url)

        if len(problems) == 0:
            return [('pass', '', url + case) for case, _ in cases]
        else:
            return problems
