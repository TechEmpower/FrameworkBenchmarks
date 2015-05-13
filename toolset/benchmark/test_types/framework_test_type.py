import copy
import sys
import subprocess
from subprocess import PIPE

from pprint import pprint

class FrameworkTestType:
  '''Interface between a test type (json, query, plaintext, etc) and 
  the rest of TFB. A test type defines a number of keys it expects
  to find in the benchmark_config.json, and this base class handles extracting
  those keys and injecting them into the test. For example, if 
  benchmark_config.json contains a line `"spam" : "foobar"` and a subclasses X
  passes an argument list of ['spam'], then after parsing there will 
  exist a member `X.spam = 'foobar'`. 
  '''

  accept_json = "Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
  accept_html = "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  accept_plaintext = "Accept: text/plain,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"

  def __init__(self, name, requires_db = False, accept_header = None, args = []):
    self.name = name
    self.requires_db = requires_db
    self.args = args
    self.out = sys.stdout
    self.err = sys.stderr
    self.accept_header = accept_header
    if accept_header is None:
      self.accept_header = self.accept_plaintext

    self.passed = None
    self.failed = None
    self.warned = None

  def setup_out_err(self, out, err):
    '''Sets up file-like objects for logging. Used in 
    cases where it is hard just return the output. Any
    output sent to these file objects is also printed to 
    the console

    NOTE: I detest this. It would be much better to use
    logging like it's intended
    '''
    self.out = out
    self.err = err
  
  def parse(self, test_keys):
    '''Takes the dict of key/value pairs describing a FrameworkTest 
    and collects all variables needed by this FrameworkTestType

    Raises AttributeError if required keys are missing
    '''
    if all (arg in test_keys for arg in self.args):
      self.__dict__.update({ arg:test_keys[arg] for arg in self.args})
      return self
    else: # This is quite common - most tests don't support all types
      raise AttributeError("A %s requires the benchmark_config.json to contain %s"%(self.name,self.args))

  def _curl(self, url):
    '''Downloads a URL and returns the HTTP response'''
    # Use -m 15 to make curl stop trying after 15sec.
    # Use -i to output response with headers
    # Don't use -f so that the HTTP response code is ignored.
    # Use -sS to hide progress bar, but show errors.
    print "Accessing URL %s:" % url
    self.err.write("Accessing URL %s \n" % url)
    self.out.write("Accessing URL %s \n" % url)
    p = subprocess.Popen(["curl", "-m", "15", "-i", "-sS", url], stderr=PIPE, stdout=PIPE)
    (out, err) = p.communicate()
    self.err.write(err+'\n')
    self.out.write("Response: \n\"" + out.strip() + "\"\n")
    if p.returncode != 0:
      return None
    return out

  def _curl_body(self, url):
    '''Downloads a URL and returns the HTTP body'''
    # Use -m 15 to make curl stop trying after 15sec.
    # Don't use -f so that the HTTP response code is ignored.
    # Use -s to hide progress bar
    # Get response body
    p = subprocess.Popen(["curl", "-m", "15", "-s", url], stdout=PIPE, stderr=PIPE)
    (out, err) = p.communicate()
    print "  Response (trimmed to 40 bytes): \"%s\"" % out.strip().replace('\n','')[:40]
    return out
  
  def verify(self, base_url):
    '''Accesses URL used by this test type and checks the return 
    values for correctness. Most test types run multiple checks,
    so this returns a list of results. Each result is a 3-tuple
    of (String result, String reason, String urlTested).

    - result : 'pass','warn','fail'
    - reason : Short human-readable reason if result was 
        warn or fail. Please do not print the response as part of this, 
        other parts of TFB will do that based upon the current logging 
        settings if this method indicates a failure happened
    - urlTested: The exact URL that was queried

    Subclasses should make a best-effort attempt to report as many
    failures and warnings as they can to help users avoid needing 
    to run TFB repeatedly while debugging
    '''
    # TODO make String result into an enum to enforce
    raise NotImplementedError("Subclasses must provide verify")

  def get_url(self):
    '''Returns the URL for this test, like '/json'''
    # This is a method because each test type uses a different key
    # for their URL so the base class can't know which arg is the URL
    raise NotImplementedError("Subclasses must provide verify")

  def copy(self):
    '''Returns a copy that can be safely modified. Use before calling 
    parse'''
    return copy.copy(self)

