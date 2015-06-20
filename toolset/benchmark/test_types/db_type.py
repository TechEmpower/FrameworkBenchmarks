from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.verifications import verify_headers

import json


class DBTestType(FrameworkTestType):

    def __init__(self):
        kwargs = {
            'name': 'db',
            'accept_header': self.accept('json'),
            'requires_db': True,
            'args': ['db_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.db_url

    def verify(self, base_url):
        '''Ensures body is valid JSON with a key 'id' and a key 
        'randomNumber', both of which must map to integers
        '''

        url = base_url + self.db_url
        headers, body = self.request_headers_and_body(url)

        # Empty response
        if body is None:
            return [('fail', 'No response', url)]
        elif len(body) == 0:
            return [('fail', 'Empty Response', url)]

        # Valid JSON?
        try:
            response = json.loads(body)
        except ValueError as ve:
            return [('fail', "Invalid JSON - %s" % ve, url)]

        problems = []

        # We are allowing the single-object array
        # e.g. [{'id':5, 'randomNumber':10}] for now,
        # but will likely make this fail at some point
        if type(response) == list:
            response = response[0]
            problems.append(
                ('warn', 'Response is a JSON array. Expected JSON object (e.g. [] vs {})', url))

            # Make sure there was a JSON object inside the array
            if type(response) != dict:
                problems.append(
                    ('fail', 'Response is not a JSON object or an array of JSON objects', url))
                return problems

        # Verify response content
        problems += self._verifyObject(response, url)
        problems += verify_headers(headers, url, should_be='json')

        if len(problems) == 0:
            return [('pass', '', url)]
        else:
            return problems

    # Prime refactor target. This method is also utilized by the multiple queries
    # and updates tests, yet is not separated out nicely.
    def _verifyObject(self, db_object, url, max_infraction='fail'):
        '''Ensure the passed item is a JSON object with 
        keys 'id' and 'randomNumber' mapping to ints. 
        Separate method allows the QueryTestType to 
        reuse these checks'''

        problems = []

        # Dict is expected, handle bytes in non-cases
        if type(db_object) is not dict:
            got = str(db_object)[:20]
            if len(str(db_object)) > 20:
                got = str(db_object)[:17] + '...'
            return [(max_infraction, "Expected a JSON object, got '%s' instead" % got, url)]

        # Make keys case insensitive
        db_object = {k.lower(): v for k, v in db_object.iteritems()}
        required_keys = set(['id', 'randomnumber'])

        if any(v not in db_object for v in required_keys):
            problems.append(
                (max_infraction, 'Response object was missing required key: %s' % v, url))

        if len(db_object) > len(required_keys):
            extras = db_object.keys() - required_keys
            problems.append(
                ('warn',
                 'An extra key(s) is being included with the db object: ' + ', '.join(extras),
                 url))

        # All required keys must be present
        if len(problems) > 0:
            return problems

        # Assert key types and values
        try:
            o_id = int(db_object['id'])
            
            if o_id > 10000 or o_id < 1:
                problems.append(
                    ('warn',
                     'Response key id should be between 1 and 10,000: ' + str(o_id),
                     url))
        except TypeError as e:
            problems.append(
                (max_infraction, "Response key 'id' does not map to an integer - %s" % e, url))

        try:
            o_rn = int(db_object['randomnumber'])
            
            if o_rn > 10000:
                problems.append(
                    ('warn',
                     'Response key `randomNumber` is over 10,000. This may negatively affect performance by sending extra bytes',
                     url))
        except TypeError as e:
            problems.append(
                (max_infraction, "Response key 'randomnumber' does not map to an integer - %s" % e, url))

        return problems
