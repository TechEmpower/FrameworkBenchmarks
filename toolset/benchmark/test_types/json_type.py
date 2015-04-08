from benchmark.test_types.framework_test_type import FrameworkTestType

import json

class JsonTestType(FrameworkTestType):
  def __init__(self):
    args = ['json_url']
    FrameworkTestType.__init__(self, name='json', requires_db=False, accept_header=self.accept_json, args=args)

  def get_url(self):
    return self.json_url

  def verify(self, base_url):
    '''Validates the response is a JSON object of 
    { 'message' : 'hello, world!' }. Case insensitive and 
    quoting style is ignored
    '''

    url = base_url + self.json_url
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
    
    # Make everything case insensitive
    response = {k.lower(): v.lower() for k,v in response.iteritems()}

    if "message" not in response:
      return [('fail',"No JSON key 'message'", url)]

    if len(response) != 1:
      return [('warn',"Too many JSON key/value pairs, expected 1", url)]

    if response['message'] != 'hello, world!':
      return [('fail',"Expected message of 'hello, world!', got '%s'"%response['message'], url)]

    # Ensure required response headers are present
    if any(v.lower() not in full_response.lower() for v in ('Server','Date','Content-Type: application/json')) \
       or all(v.lower() not in full_response.lower() for v in ('Content-Length','Transfer-Encoding')):
      return [('warn','Required response header missing.',url)]

    return [('pass','',url)]
