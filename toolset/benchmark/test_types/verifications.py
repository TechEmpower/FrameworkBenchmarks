import json


def verify_headers(headers, url, should_be='json'):
    '''
    Verifies the headers of a framework response
    param `should_be` is a switch for the three acceptable content types
    '''

    types = {
        'json': 'application/json',
        'html': 'text/html',
        'plaintext': 'text/plain'
    }
    expected_type = types[should_be]
    includes_charset = expected_type + '; charset=utf-8'

    problems = []

    if any(v.lower() not in headers for v in ('Server', 'Date', 'Content-Type')):
        problems.append(
            ('warn', 'Required response header missing: %s' % v, url))
    elif all(v.lower() not in headers for v in ('Content-Length', 'Transfer-Encoding')):
        problems.append(
            ('warn',
             'Required response size header missing, please include either "Content-Length" or "Transfer-Encoding"',
             url))
    else:
        content_type = headers.get('Content-Type', None)

        if content_type.lower() == includes_charset:
            problems.append(
                ('warn',
                 ("Content encoding found \"%s\" where \"%s\" is acceptable.\n"
                  "Additional response bytes may negatively affect benchmark performance."
                  % (includes_charset, expected_type)),
                 url))
        elif content_type != expected_type:
            problems.append(
                ('warn',
                 'Unexpected content encoding, found \"%s\", expected \"%s\"' % (
                     content_type, expected_type),
                 url))
    return problems


def verify_helloworld_object(json_object, url):
    '''
    Ensure that the JSON object closely resembles
    { 'message': 'Hello, World!' }
    '''

    problems = []

    # Make everything case insensitive
    json_object = {k.lower(): v.lower()
                   for k, v in json_object.iteritems()}

    if 'message' not in json_object:
        return [('fail', "Missing required key 'message'", url)]
    else:
        if len(json_object) > 1:
            additional = (', ').join(
                [k for k in json_object.keys() if k != 'message'])
            problems.append(
                ('warn', "Too many JSON key/value pairs, consider removing: %s" % additional, url))

        message = json_object['message']

        if message != 'hello, world!':
            return [('fail', "Expected message of 'hello, world!', got '%s'" % message)]
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
             'An extra key(s) is being included with the db object: %s' % ', '.join(
                 extras),
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
                 'Response key id should be between 1 and 10,000: ' +
                 str(o_id),
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


def verify_randomnumber_list(expected_len, headers, body, url, max_infraction='fail'):
    '''
    Validates that the object is a list containing a number of 
    randomnumber object. Should closely resemble:
    [{ "id": 2354, "randomNumber": 8952 }, { id: }]
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
        problems += verify_randomnumber_object(response, url, max_infraction)
        return problems

    if any(type(item) is not dict for item in response):
        problems.append(
            (max_infraction, 'Not all items in the JSON array were JSON objects', url))

    if len(response) != expected_len:
        problems.append(
            (max_infraction,
             "JSON array length of %s != expected length of %s" % (
                 len(response), expected_len),
             url))

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
