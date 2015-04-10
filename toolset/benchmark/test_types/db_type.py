from benchmark.test_types.framework_test_type import FrameworkTestType

import json

class DBTestType(FrameworkTestType):
  def __init__(self):
    args = ['db_url']
    FrameworkTestType.__init__(self, name='db', 
      accept_header=self.accept_json,
      requires_db=True, args=args)

  def get_url(self):
    return self.db_url

  def verify(self, base_url):
    '''Ensures body is valid JSON with a key 'id' and a key 
    'randomNumber', both of which must map to integers
    '''

    url = base_url + self.db_url
    full_response = self._curl(url)
    body = self._curl_body(url)
    
    # Empty response
    if body is None:
      return [('fail','No response', url)]
    elif len(body) == 0:
      return [('fail','Empty Response', url)]

    # Valid JSON? 
    try: 
      response = json.loads(body)
    except ValueError as ve:
      return [('fail',"Invalid JSON - %s" % ve, url)]

    problems = []

    # We are allowing the single-object array 
    # e.g. [{'id':5, 'randomNumber':10}] for now, 
    # but will likely make this fail at some point
    if type(response) == list:
      response = response[0]
      problems.append( ('warn', 'Response is a JSON array. Expected JSON object (e.g. [] vs {})', url) ) 

      # Make sure there was a JSON object inside the array
      if type(response) != dict:
        problems.append( ('fail', 'Response is not a JSON object or an array of JSON objects', url) ) 
        return problems

    problems += self._verifyObject(response, url)

    # Ensure required response headers are present
    if any(v.lower() not in full_response.lower() for v in ('Server','Date','Content-Type: application/json')) \
       or all(v.lower() not in full_response.lower() for v in ('Content-Length','Transfer-Encoding')):
      problems.append( ('warn','Required response header missing.',url) )

    if len(problems) == 0:
      return [('pass','',url)]
    else:
      return problems

  def _verifyObject(self, db_object, url, max_infraction='fail'):
    '''Ensure the passed item is a JSON object with 
    keys 'id' and 'randomNumber' mapping to ints. 
    Separate method allows the QueryTestType to 
    reuse these checks'''

    problems = []

    if type(db_object) != dict:
      got = str(db_object)[:20]
      if len(str(db_object)) > 20:
        got = str(db_object)[:17] + '...'
      return [(max_infraction, "Expected a JSON object, got '%s' instead" % got, url)]

    # Make keys case insensitive
    db_object = {k.lower(): v for k,v in db_object.iteritems()}

    if "id" not in db_object:
      problems.append( (max_infraction, "Response has no 'id' key", url) ) 
    if "randomnumber" not in db_object:
      problems.append( (max_infraction, "Response has no 'randomNumber' key", url) )
    
    # Ensure we can continue on to use these keys
    if "id" not in db_object or "randomnumber" not in db_object:
      return problems

    try:
      float(db_object["id"])
    except ValueError as ve:
      problems.append( (max_infraction, "Response key 'id' does not map to a number - %s" % ve, url) ) 

    try:
      float(db_object["randomnumber"])
    except ValueError as ve:
      problems.append( (max_infraction, "Response key 'randomNumber' does not map to a number - %s" % ve, url) ) 

    if type(db_object["id"]) != int:
      problems.append( ('warn', '''Response key 'id' contains extra quotations or decimal points.
        This may negatively affect performance during benchmarking''', url) ) 

    # Tests based on the value of the numbers
    try:
      response_id = float(db_object["id"])
      response_rn = float(db_object["randomnumber"])

      if response_id > 10000 or response_id < 1:
        problems.append( ('warn', "Response key 'id' should be between 1 and 10,000", url) ) 

      if response_rn > 10000:
        problems.append( ('warn', '''Response key 'randomNumber' is over 10,000. This may negatively 
          afect performance by sending extra bytes.''', url) )
    except ValueError:
      pass

    return problems

