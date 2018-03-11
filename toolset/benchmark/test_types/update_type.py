from toolset.benchmark.test_types.framework_test_type import FrameworkTestType
from toolset.benchmark.test_types.verifications import verify_query_cases
from toolset.utils.remote_script_helper import generate_query_script


class UpdateTestType(FrameworkTestType):
    def __init__(self, config):
        self.update_url = ""
        kwargs = {
            'name': 'update',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['update_url', 'database']
        }
        FrameworkTestType.__init__(self, config, **kwargs)

    def get_url(self):
        return self.update_url

    def verify(self, base_url):
        '''
        Validates the response is a JSON array of 
        the proper length, each JSON Object in the array 
        has keys 'id' and 'randomNumber', and these keys 
        map to integers. Case insensitive and 
        quoting style is ignored
        '''

        url = base_url + self.update_url
        cases = [('2', 'fail'), ('0', 'fail'), ('foo', 'fail'),
                 ('501', 'warn'), ('', 'fail')]
        problems = verify_query_cases(self, cases, url, True)

        if len(problems) == 0:
            return [('pass', '', url + case) for (case, _) in cases]
        else:
            return problems

    def get_remote_script(self, config, name, url, port):
        '''
        Returns the remote script
        '''
        return generate_query_script(self.config, name, url, port,
                                     self.accept_header,
                                     self.config.query_levels)
