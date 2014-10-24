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
    body = self._curl(url)
    problems = []

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

    # Make keys case insensitive
    response = {k.lower(): v for k,v in response.iteritems()}

    if "id" not in response:
      problems.append( ('fail', "Response has no 'id' key", url) ) 
    if "randomnumber" not in response:
      problems.append( ('fail', "Response has no 'randomNumber' key", url) ) 

    try:
      float(response["id"])
    except ValueError as ve:
      problems.append( ('fail', "Response key 'id' does not map to a number - %s" % ve, url) ) 

    try:
      float(response["randomnumber"])
    except ValueError as ve:
      problems.append( ('fail', "Response key 'randomNumber' does not map to a number - %s" % ve, url) ) 

    if type(response["id"]) != int:
      problems.append( ('warn', '''Response key 'id' contains extra quotations or decimal points.
        This may negatively affect performance during benchmarking''', url) ) 

    # Tests based on the value of the numbers
    try:
      response_id = float(response["id"])
      response_rn = float(response["randomnumber"])

      if response_id > 10000 or response_id < 1:
        problems.append( ('warn', "Response key 'id' should be between 1 and 10,000", url)) 
    except ValueError:
      pass

    if len(problems) == 0:
      return [('pass','',url)]
    else:
      return problems