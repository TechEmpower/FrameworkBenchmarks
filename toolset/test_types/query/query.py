from toolset.test_types.abstract_test_type import AbstractTestType
from toolset.test_types.verifications import verify_query_cases


class TestType(AbstractTestType):
    def __init__(self, config):
        self.query_url = ""
        kwargs = {
            'name': 'query',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['query_url', 'database']
        }
        AbstractTestType.__init__(self, config, **kwargs)

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

        problems = verify_query_cases(self, cases, url, False)

        # queries_url should be at least "/queries/"
        # some frameworks use a trailing slash while others use ?q=
        if len(self.query_url) < 9:
            problems.append(
                ("fail",
                 "Route for queries must be at least 9 characters, found '{}' instead".format(self.query_url),
                 url))

        if len(problems) == 0:
            return [('pass', '', url + case) for case, _ in cases]
        else:
            return problems

    def get_script_name(self):
        return 'query.sh'

    def get_script_variables(self, name, url):
        return {
            'max_concurrency':
            max(self.config.concurrency_levels),
            'name':
            name,
            'duration':
            self.config.duration,
            'levels':
            " ".join("{}".format(item) for item in self.config.query_levels),
            'server_host':
            self.config.server_host,
            'url':
            url,
            'accept':
            "application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
        }
