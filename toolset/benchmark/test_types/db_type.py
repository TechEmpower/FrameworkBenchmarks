from toolset.benchmark.test_types.framework_test_type import FrameworkTestType
from toolset.benchmark.test_types.verifications import basic_body_verification, verify_headers, verify_randomnumber_object


class DBTestType(FrameworkTestType):
    def __init__(self, config):
        self.db_url = ""
        kwargs = {
            'name': 'db',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['db_url']
        }
        FrameworkTestType.__init__(self, config, **kwargs)

    def get_url(self):
        return self.db_url

    def verify(self, base_url):
        '''
        Ensures body is valid JSON with a key 'id' and a key 
        'randomNumber', both of which must map to integers
        '''

        url = base_url + self.db_url
        headers, body = self.request_headers_and_body(url)

        response, problems = basic_body_verification(body, url)

        if len(problems) > 0:
            return problems

        # We are allowing the single-object array
        # e.g. [{'id':5, 'randomNumber':10}] for now,
        # but will likely make this fail at some point
        if type(response) == list:
            response = response[0]
            problems.append((
                'warn',
                'Response is a JSON array. Expected JSON object (e.g. [] vs {})',
                url))

            # Make sure there was a JSON object inside the array
            if type(response) != dict:
                problems.append((
                    'fail',
                    'Response is not a JSON object or an array of JSON objects',
                    url))
                return problems

        # Verify response content
        problems += verify_randomnumber_object(response, url)
        problems += verify_headers(headers, url, should_be='json')

        if len(problems) == 0:
            return [('pass', '', url)]
        else:
            return problems

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
