import json
import re
import traceback
import multiprocessing

from datetime import datetime
from toolset.utils.output_helper import log
from toolset.databases import databases
from time import sleep

# Cross-platform colored text
from colorama import Fore, Style


def basic_body_verification(body, url, is_json_check=True):
    '''
    Takes in a raw (stringy) response body, checks that it is non-empty,
    and that it is valid JSON (i.e. can be deserialized into a dict/list of dicts)
    Returns the deserialized body as a dict (or list of dicts), and also returns any
    problems encountered, always as a list. If len(problems) > 0,
    then the response body does not have to be examined further and the caller
    should handle the failing problem(s).
    Plaintext and Fortunes set `is_json_check` to False
    '''

    # Empty Response?
    if body is None:
        return None, [('fail', 'No response', url)]
    elif len(body) == 0:
        return None, [('fail', 'Empty response', url)]

    # Valid JSON?
    if is_json_check:
        try:
            response = json.loads(body)
            return response, []
        except ValueError as ve:
            return None, [('fail', 'Invalid JSON: %s' % ve, url)]

    # Fortunes and Plaintext only use this for the empty response tests
    # they do not need or expect a dict back
    return None, []


def verify_headers(request_headers_and_body, headers, url, should_be='json'):
    '''
    Verifies the headers of a framework response
    param `should_be` is a switch for the three acceptable content types
    '''

    problems = []

    for v in (v for v in ('Server', 'Date', 'Content-Type')
              if v.lower() not in headers):
        problems.append(('fail', 'Required response header missing: %s' % v,
                         url))

    if all(v.lower() not in headers
           for v in ('Content-Length', 'Transfer-Encoding')):
        problems.append((
            'fail',
            'Required response size header missing, please include either "Content-Length" or "Transfer-Encoding"',
            url))

    date = headers.get('Date')
    if date is not None:
        expected_date_format = '%a, %d %b %Y %H:%M:%S %Z'
        try:
            datetime.strptime(date, expected_date_format)
        except ValueError:
            problems.append((
                'warn',
                'Invalid Date header, found \"%s\", did not match \"%s\".'
                % (date, expected_date_format), url))

    # Verify response content
    # Make sure that the date object isn't cached
    sleep(3)
    second_headers, body2 = request_headers_and_body(url)
    second_date = second_headers.get('Date')

    date2 = second_headers.get('Date')
    if date == date2:
        problems.append((
            'fail',
            'Invalid Cached Date. Found \"%s\" and \"%s\" on separate requests.'
            % (date, date2), url))

    content_type = headers.get('Content-Type')
    if content_type is not None:
        types = {
            'json': '^application/json(; ?charset=(UTF|utf)-8)?$',
            'html': '^text/html; ?charset=(UTF|utf)-8$',
            'plaintext': '^text/plain(; ?charset=(UTF|utf)-8)?$'
        }
        expected_type = types[should_be]

        if not re.match(expected_type, content_type):
            problems.append((
                'fail',
                'Invalid Content-Type header, found \"%s\", did not match \"%s\".'
                % (content_type, expected_type), url))

    return problems


def verify_helloworld_object(json_object, url):
    '''
    Ensure that the JSON object closely resembles
    { 'message': 'Hello, World!' }
    '''

    problems = []

    try:
        # Make everything case insensitive
        json_object = {k.lower(): v.lower() for k, v in json_object.iteritems()}
    except:
        return [('fail', "Not a valid JSON object", url)]

    if 'message' not in json_object:
        return [('fail', "Missing required key 'message'", url)]
    else:
        json_len = len(json_object)
        if json_len > 1:
            additional = ', '.join(
                [k for k in json_object.keys() if k != 'message'])
            problems.append(
                ('warn', "Too many JSON key/value pairs, consider removing: %s"
                 % additional, url))
        if json_len > 27:
            problems.append(
                'warn',
                "%s additional response byte(s) found. Consider removing unnecessary whitespace."
                % (json_len - 26))
        message = json_object['message']

        if message != 'hello, world!':
            return [('fail',
                     "Expected message of 'hello, world!', got '%s'" % message,
                     url)]
        return problems


def verify_randomnumber_object(db_object, url, max_infraction='fail'):
    '''
    Ensures that `db_object` is a JSON object with
    keys 'id' and 'randomNumber' that both map to ints.
    Should closely resemble:
    { "id": 2354, "randomNumber": 8952 }
    '''

    problems = []

    # Dict is expected
    # Produce error for bytes in non-cases
    if type(db_object) is not dict:
        got = str(db_object)[:20]
        if len(str(db_object)) > 20:
            got = str(db_object)[:17] + '...'
        return [(max_infraction,
                 "Expected a JSON object, got '%s' instead" % got, url)]

    # Make keys case insensitive
    db_object = {k.lower(): v for k, v in db_object.iteritems()}
    required_keys = set(['id', 'randomnumber'])

    for v in (v for v in required_keys if v not in db_object):
        problems.append(
            (max_infraction,
             'Response object was missing required key: %s' % v, url))

    if len(db_object) > len(required_keys):
        extras = set(db_object.keys()) - required_keys
        problems.append(
            ('warn', 'An extra key(s) is being included with the db object: %s'
             % ', '.join(extras), url))

    # All required keys must be present
    if len(problems) > 0:
        return problems

    # Assert key types and values
    try:
        o_id = int(db_object['id'])

        if o_id > 10000 or o_id < 1:
            problems.append((
                'warn',
                'Response key id should be between 1 and 10,000: ' + str(o_id),
                url))
    except TypeError as e:
        problems.append(
            (max_infraction,
             "Response key 'id' does not map to an integer - %s" % e, url))

    try:
        o_rn = int(db_object['randomnumber'])

        if o_rn > 10000:
            problems.append((
                'warn',
                'Response key `randomNumber` is over 10,000. This may negatively affect performance by sending extra bytes',
                url))
    except TypeError as e:
        problems.append(
            (max_infraction,
             "Response key 'randomnumber' does not map to an integer - %s" % e,
             url))

    return problems


def verify_randomnumber_list(expected_len,
                             headers,
                             body,
                             url,
                             max_infraction='fail'):
    '''
    Validates that the object is a list containing a number of
    randomnumber object. Should closely resemble:
    [{ "id": 2354, "randomNumber": 8952 }, { "id": 4421, "randomNumber": 32 }, ... ]
    '''

    response, problems = basic_body_verification(body, url)

    if len(problems) > 0:
        return problems

    # This path will be hit when the framework returns a single JSON object
    # rather than a list containing one element.
    if type(response) is not list:
        problems.append((max_infraction,
                         'Top-level JSON is an object, not an array',
                         url))
        problems += verify_randomnumber_object(response, url, max_infraction)
        return problems

    if any(type(item) is not dict for item in response):
        problems.append(
            (max_infraction,
             'Not all items in the JSON array were JSON objects', url))

    if len(response) != expected_len:
        problems.append((max_infraction,
                         "JSON array length of %s != expected length of %s" %
                         (len(response), expected_len), url))

    # Verify individual objects, arbitrarily stop after 5 bad ones are found
    # i.e. to not look at all 500
    badObjectsFound = 0
    inner_objects = iter(response)

    try:
        while badObjectsFound < 5:
            obj = next(inner_objects)
            findings = verify_randomnumber_object(obj, url, max_infraction)

            if len(findings) > 0:
                problems += findings
                badObjectsFound += 1
    except StopIteration:
        pass

    return problems


def verify_updates(old_worlds, new_worlds, updates_expected, url):
    '''
    Validates that the /updates requests actually updated values in the database and didn't
    just return a JSON list of the correct number of World items.

    old_worlds  a JSON object containing the state of the Worlds table BEFORE the /updates requests
    new_worlds  a JSON object containing the state of the Worlds table AFTER the /updates requests
    If no items were updated, this validation test returns a "fail."

    If only some items were updated (within a 5% margin of error), this test returns a "warn".
    This is to account for the unlikely, but possible situation where an entry in the World
    table is updated to the same value it was previously set as.
    '''
    successful_updates = 0
    problems = []

    n = 0
    while n < len(old_worlds) and successful_updates == 0:
        for i in range(1, 10001):
            try:
                entry_id = str(i)
                if entry_id in old_worlds[n] and entry_id in new_worlds[n]:
                    if old_worlds[n][entry_id] != new_worlds[n][entry_id]:
                        successful_updates += 1
            except Exception:
                tb = traceback.format_exc()
                log(tb)
        n += 1

    if successful_updates == 0:
        problems.append(("fail", "No items were updated in the database.",
                         url))
    elif successful_updates <= (updates_expected * 0.90):
        problems.append((
            "fail",
            "Only %s items were updated in the database out of roughly %s expected."
            % (successful_updates, updates_expected), url))
    elif successful_updates <= (updates_expected * 0.95):
        problems.append((
            "warn",
            "There may have been an error updating the database. Only %s items were updated in the database out of the roughly %s expected."
            % (successful_updates, updates_expected), url))

    return problems


def verify_query_cases(self, cases, url, check_updates=False):
    '''
    The /updates and /queries tests accept a `queries` parameter
    that is expected to be between 1-500.
    This method execises a framework with different `queries` parameter values
    then verifies that the framework responds appropriately.
    The `cases` parameter should be a list of 2-tuples containing the query case
    and the consequence level should the cases fail its verifications, e.g.:
    cases = [
        ('2',   'fail'),
        ('0',   'fail'),
        ('foo', 'fail'),
        ('501', 'warn'),
        ('',    'fail')
    ]
    The reason for using 'warn' is generally for a case that will be allowed in the
    current run but that may/will be a failing case in future rounds. The cases above
    suggest that not sanitizing the `queries` parameter against non-int input, or failing
    to ensure the parameter is between 1-500 will just be a warn,
    and not prevent the framework from being benchmarked.
    '''
    problems = []
    MAX = 500
    MIN = 1
    # Initialization for query counting
    repetitions = 1
    concurrency = max(self.config.concurrency_levels)
    expected_queries = 20 * repetitions * concurrency
    expected_rows = expected_queries

    # Only load in the World table if we are doing an Update verification
    world_db_before = {}
    if check_updates:
        world_db_before = databases[self.database.lower()].get_current_world_table(self.config)
        expected_queries = expected_queries + concurrency * repetitions  # eventually bulk updates!

    for q, max_infraction in cases:
        case_url = url + q
        headers, body = self.request_headers_and_body(case_url)

        try:
            queries = int(q)  # drops down for 'foo' and ''

            if queries > MAX:
                expected_len = MAX
            elif queries < MIN:
                expected_len = MIN
            else:
                expected_len = queries

            problems += verify_randomnumber_list(expected_len, headers, body,
                                                 case_url, max_infraction)
            problems += verify_headers(self.request_headers_and_body, headers, case_url)

            # Only check update changes if we are doing an Update verification and if we're testing
            # the highest number of queries, to ensure that we don't accidentally FAIL for a query
            # that only updates 1 item and happens to set its randomNumber to the same value it
            # previously held
            if check_updates and queries >= MAX:
                world_db_after = databases[self.database.lower()].get_current_world_table(self.config)
                problems += verify_updates(world_db_before, world_db_after,
                                           MAX, case_url)

        except ValueError:
            warning = (
                '%s given for stringy `queries` parameter %s\n'
                'Suggestion: modify your /queries route to handle this case '
                '(this will be a failure in future rounds, please fix)')

            if body is None:
                problems.append((max_infraction, warning % ('No response', q),
                                 case_url))
            elif len(body) == 0:
                problems.append((max_infraction, warning % ('Empty response',
                                                            q), case_url))
            else:
                expected_len = 1
                # Strictness will be upped in a future round, i.e. Frameworks currently do not have
                # to gracefully handle absent, or non-intlike `queries`
                # parameter input
                problems += verify_randomnumber_list(
                    expected_len, headers, body, case_url, max_infraction)
                problems += verify_headers(self.request_headers_and_body, headers, case_url)

    if hasattr(self, 'database'):
        # verify the number of queries and rows read for 20 queries, with a concurrency level of 512, with 2 repetitions
        problems += verify_queries_count(self, "world", url + "20", concurrency, repetitions, expected_queries,
                                         expected_rows, check_updates)
    return problems


def verify_queries_count(self, tbl_name, url, concurrency=512, count=2, expected_queries=1024, expected_rows=1024,
                         check_updates=False):
    '''
    Checks that the number of executed queries, at the given concurrency level,
    corresponds to: the total number of http requests made * the number of queries per request.
    No margin is accepted on the number of queries, which seems reliable.
    On the number of rows read or updated, the margin related to the database applies (1% by default see cls.margin)
    On updates, if the use of bulk updates is detected (number of requests close to that expected), a margin
    (5% see bulk_margin) is allowed on the number of updated rows.
    '''
    log("VERIFYING QUERY COUNT FOR %s" % url, border='-', color=Fore.WHITE + Style.BRIGHT)

    problems = []

    queries, rows, rows_updated, margin, trans_failures = databases[self.database.lower()].verify_queries(self.config,
                                                                                                          tbl_name, url,
                                                                                                          concurrency,
                                                                                                          count,
                                                                                                          check_updates)

    isBulk = check_updates and (queries < 1.001 * expected_queries) and (queries > 0.999 * expected_queries)

    if check_updates and not isBulk:  # Restore the normal queries number if bulk queries are not used
        expected_queries = (expected_queries - count * concurrency) * 2

    # Add a margin based on the number of cpu cores
    queries_margin = 1.015  # For a run on Travis
    if multiprocessing.cpu_count() > 2:
        queries_margin = 1  # real run (Citrine or Azure) -> no margin on queries
        # Check for transactions failures (socket errors...)
        if trans_failures > 0:
            problems.append((
                "fail",
                "%s failed transactions."
                % trans_failures, url))

    problems.append(
        display_queries_count_result(queries * queries_margin, expected_queries, queries, "executed queries", url))

    problems.append(display_queries_count_result(rows, expected_rows, int(rows / margin), "rows read", url))

    if check_updates:
        bulk_margin = 1
        if isBulk:  # Special marge for bulk queries
            bulk_margin = 1.05
        problems.append(
            display_queries_count_result(rows_updated * bulk_margin, expected_rows, int(rows_updated / margin),
                                         "rows updated", url))

    return problems


def display_queries_count_result(result, expected_result, displayed_result, caption, url):
    '''
    Returns a single result in counting queries, rows read or updated.
    result corresponds to the effective result adjusted by the margin.
    displayed_result is the effective result (without correction).
    '''
    if result > expected_result * 1.05:
        return (
            "warn",
            "%s %s in the database instead of %s expected. This number is excessively high."
            % (displayed_result, caption, expected_result), url)
    elif result < expected_result:
        return (
            "fail",
            "Only %s %s in the database out of roughly %s expected."
            % (displayed_result, caption, expected_result), url)
    else:
        return ("pass", "%s: %s/%s" % (caption.capitalize(), displayed_result, expected_result), url)
