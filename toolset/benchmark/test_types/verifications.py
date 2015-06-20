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
