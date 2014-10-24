import os
import glob
import json
import copy
import subprocess
from subprocess import PIPE
import sys
import json

from pprint import pprint

class FrameworkTestType:
  '''Interface between a test type (json, query, plaintext, etc) and 
  the rest of TFB. A test type defines a number of keys it expects
  to find in the benchmark_config, and this base class handles extracting
  those keys and injecting them into the test. For example, if 
  benchmark_config contains a line `"spam" : "foobar"` and a subclasses X
  passes an argument list of ['spam'], then after parsing there will 
  exist a member `X.spam = 'foobar'`. 
  '''

  def __init__(self, name, requires_db = False, args = []):
    self.name = name
    self.requires_db = requires_db
    self.args = args
    self.out = [] # You can use [sys.stdout] to tee
    self.err = [] # [sys.stderr]

  def setup_out_err(self, out, err):
    '''Sets up file-like objects for logging. Used in 
    cases where it is hard just return the output. Any
    output sent to these file objects is also printed to 
    the console

    NOTE: I detest this. It would be much better to use
    logging like it's intended
    '''
    self.out.append(out)
    self.err.append(err)
  
  def parse(self, test_keys):
    '''Takes the dict of key/value pairs describing a FrameworkTest 
    and collects all variables needed by this FrameworkTestType

    Raises AttributeError if required keys are missing
    '''
    if all (arg in test_keys for arg in self.args):
      self.__dict__.update({ arg:test_keys[arg] for arg in self.args})
      return self
    else: # This is quite common - most tests don't support all types
      raise AttributeError("A %s requires the benchmark_config to contain %s"%(self.name,self.args))

  def _curl(self, url):
    '''Downloads a URL and returns the HTTP body'''
    # Use -m 15 to make curl stop trying after 15sec.
    # Use -i to output response with headers
    # Don't use -f so that the HTTP response code is ignored.
    # Use -sS to hide progress bar, but show errors.
    p = subprocess.Popen(["curl", "-m", "15", "-i", "-sS", url], stderr=PIPE, stdout=PIPE)
    (out, err) = p.communicate()
    [item.write(err+'\n') for item in self.err]
    [item.write(out+'\n') for item in self.out]
    if p.returncode != 0:
      return None
    # Get response body
    p = subprocess.Popen(["curl", "-m", "15", "-s", url], stdout=PIPE, stderr=PIPE)
    (out, err) = p.communicate()
    [item.write(err+'\n') for item in self.err]
    return out
  
  def copy(self):
    '''Returns a copy that can be safely modified. Use before calling 
    parse'''
    return copy.copy(self)

  def verify(self, base_url):
    '''Accesses URL used by this test type and checks the return 
    values for correctness. Most test types run multiple checks,
    so this returns a list of results. Each result is a 3-tuple
    of (String result, String message, String urlTested).

    - Result is always 'pass','warn','fail'
    - message is a human-readable reason if the result was warn or fail
    - urlTested is the URL that was queried
    '''
    # TODO make String result into an enum to enforce
    # raise NotImplementedError("Subclasses must provide verify")
    return [('pass','', '')]

class JsonTestType(FrameworkTestType):
  def __init__(self):
    args = ['json_url']
    FrameworkTestType.__init__(self, 'json', False, args)

  def verify(self, base_url):
    '''Validates the response is a JSON object of 
    { 'message' : 'hello, world!' }. Case insensitive and 
    quoting style is ignored
    '''

    url = base_url + self.json_url
    body = self._curl(url)
    
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

    return [('pass','',url)]

class DBTestType(FrameworkTestType):
  def __init__(self):
    args = ['db_url']
    FrameworkTestType.__init__(self, 'db', True, args)

class QueryTestType(FrameworkTestType):
  def __init__(self):
    args = ['query_url']
    FrameworkTestType.__init__(self, 'query', True, args)


class FortuneTestType(FrameworkTestType):
  def __init__(self):
    args = ['fortune_url']
    FrameworkTestType.__init__(self, 'fortune', True, args)

class UpdateTestType(FrameworkTestType):
  def __init__(self):
    args = ['update_url']
    FrameworkTestType.__init__(self, 'update', True, args)

class PlaintextTestType(FrameworkTestType):
  def __init__(self):
    args = ['plaintext_url']
    FrameworkTestType.__init__(self, 'plaintext', False, args)
