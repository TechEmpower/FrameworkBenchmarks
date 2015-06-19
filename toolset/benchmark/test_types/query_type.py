from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.db_type import DBTestType

import json


class QueryTestType(DBTestType):

    '''
    This test type is used for the multiple queries test
    Inherits from DBTestType
    Both tests deal with the same objects; this test just expects a list of them
    '''

    def __init__(self):
        kwargs = {
            'name': 'query',
            'accept_header': self.accept_json,
            'requires_db': True,
            'args': ['query_url']
        }
        FrameworkTestType.__init__(self, **kwargs)

    def get_url(self):
        return self.query_url

    def verify(self, base_url):
        '''Validates the response is a JSON array of 
        the proper length, each JSON Object in the array 
        has keys 'id' and 'randomNumber', and these keys 
        map to integers. Case insensitive and 
        quoting style is ignored
        '''

        url = base_url + self.query_url
        cases = [
            ('2',   'fail'),
            ('0',   'warn'),
            ('foo', 'warn'),
            ('501', 'warn'),
            ('',    'warn')
        ]

        problems = self._verifyQueryCases(url, cases)

        if len(problems) == 0:
            return [('pass', '', url + case) for case, _ in cases]
        else:
            return problems

    def _verifyQueryCases(self, url, cases):
        '''
        The queries tests accepts a `queries` parameter that should be between 1-500
        This method execises a framework with different `queries` parameter values
        then verifies the framework's response.
        '''
        problems = []
        Q_MAX = 500
        Q_MIN = 1

        for q, max_infraction in cases:
            case_url = url + q
            headers, body = self.request_headers_and_body(case_url)

            try:
                queries = int(q)  # drops down for 'foo' and ''

                if queries > Q_MAX:
                    expected_len = Q_MAX
                elif queries < Q_MIN:
                    expected_len = Q_MIN
                else:
                    expected_len = queries

                problems += self._verifyBodyList(
                    expected_len, headers, body, case_url, max_infraction)
                problems += self._verifyHeaders(headers, case_url)

            except ValueError:
                warning = (
                    '%s given for stringy `queries` parameter %s\n'
                    'Suggestion: modify your /queries route to handle this case '
                    '(this will be a failure in future rounds, please fix)')

                if body is None:
                    problems.append(
                        (max_infraction,
                         warning % ('No response', q),
                         case_url))
                elif len(body) == 0:
                    problems.append(
                        (max_infraction,
                         warning % ('Empty response', q),
                         case_url))
                else:
                    expected_len = 1
                    # Strictness will be upped in a future round, i.e. Frameworks currently do not have
                    # to gracefully handle absent, or non-intlike `queries`
                    # parameter input
                    problems += self._verifyBodyList(
                        expected_len, headers, body, case_url, max_infraction)
                    problems += self._verifyHeaders(headers, case_url)

        return problems

    def _verifyBodyList(self, expectedLen, headers, body, url, max_infraction='fail'):
        '''
        Validates high-level structure (array length, object types, etc),
        then verifies the inner objects of the list for correctness
        '''
        if body is None:
            return [(max_infraction, 'No response', url)]
        elif len(body) == 0:
            return [(max_infraction, 'Empty Response', url)]

        try:
            response = json.loads(body)
        except ValueError as ve:
            return [(max_infraction, "Invalid JSON - %s" % ve, url)]

        problems = []

        # This path will be hit when the framework returns a single JSON object
        # rather than a list containing one element. We allow this with a warn,
        # then verify the supplied object
        if type(response) is not list:
            problems.append(
                ('warn', 'Top-level JSON is an object, not an array', url))
            problems += self._verifyObject(response, url, max_infraction)
            return problems

        if any(type(item) is not dict for item in response):
            problems.append(
                (max_infraction, 'Not all items in the JSON array were JSON objects', url))

        if len(response) != expectedLen:
            problems.append(
                (max_infraction,
                 "JSON array length of %s != expected length of %s" % (
                     len(response), expectedLen),
                 url))

        # Verify individual objects, arbitrarily stop after 5 bad ones are found
        # i.e. to not look at all 500
        badObjectsFound = 0
        i = iter(response)

        try:
            while badObjectsFound < 5:
                obj = next(i)
                findings = self._verifyObject(obj, url, max_infraction)

                if len(findings) > 0:
                    problems += findings
                    badObjectsFound += 1
        except StopIteration:
            pass

        return problems
