from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.db_type import DBTestType

import json

class QueryTestType(DBTestType):
  def __init__(self):
    args = ['query_url']
    FrameworkTestType.__init__(self, name='query', requires_db=True, 
      accept_header=self.accept_json, args=args)

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

    problems = []
    
    body = self._curl(url + '2')
    problems += self._verifyQueryList(2, body, url + '2')

    body = self._curl(url + '0')
    problems += self._verifyQueryList(1, body, url + '0', 'warn')

    # Note: A number of tests fail here because they only parse for 
    # a number and crash on 'foo'. For now we only warn about this
    body = self._curl(url + 'foo')
    if body is None:
      problems += [('warn','No response (this will be a failure in future rounds, please fix)', url)]
    elif len(body) == 0:
      problems += [('warn','Empty response (this will be a failure in future rounds, please fix)', url)]
    else:
      problems += self._verifyQueryList(1, body, url + 'foo', 'warn')

    body = self._curl(url + '501')
    problems += self._verifyQueryList(500, body, url + '501', 'warn')

    if len(problems) == 0:
      return [('pass','',url + '2'),
              ('pass','',url + '0'),
              ('pass','',url + 'foo'),
              ('pass','',url + '501')]
    else:
      return problems

  def _verifyQueryList(self, expectedLength, body, url, max_infraction='fail'):
    '''Validates high-level structure (array length, object 
      types, etc) before calling into DBTestType to 
      validate a few individual JSON objects'''

    # Empty response
    if body is None:
      return [(max_infraction,'No response', url)]
    elif len(body) == 0:
      return [(max_infraction,'Empty Response', url)]
  
    # Valid JSON? 
    try: 
      response = json.loads(body)
    except ValueError as ve:
      return [(max_infraction,"Invalid JSON - %s" % ve, url)]

    problems = []

    if type(response) != list:
      problems.append(('warn','Top-level JSON is an object, not an array', url))

      # Verify the one object they gave us before returning
      problems += self._verifyObject(response, url)

      return problems

    if any(type(item) != dict for item in response):
      problems.append((max_infraction,'All items JSON array must be JSON objects', url))

    # For some edge cases we only warn
    if len(response) != expectedLength:
      problems.append((max_infraction,
        "JSON array length of %s != expected length of %s" % (len(response), expectedLength), 
        url))

    # verify individual objects
    maxBadObjects = 5
    for item in response:
      obj_ok = self._verifyObject(item, url)
      if len(obj_ok) > 0:
        maxBadObjects -=  1
        problems += obj_ok
        if maxBadObjects == 0:
          break

    return problems



