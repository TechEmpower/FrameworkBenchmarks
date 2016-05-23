import copy
import sys
import subprocess
from subprocess import PIPE
import requests

# Requests is built ontop of urllib3,
# here we prevent general request logging
import logging
logging.getLogger('urllib3').setLevel(logging.CRITICAL)

from pprint import pprint


class FrameworkTestType:

    '''
    Interface between a test type (json, query, plaintext, etc) and 
    the rest of TFB. A test type defines a number of keys it expects
    to find in the benchmark_config.json, and this base class handles extracting
    those keys and injecting them into the test. For example, if 
    benchmark_config.json contains a line `"spam" : "foobar"` and a subclasses X
    passes an argument list of ['spam'], then after parsing there will 
    exist a member `X.spam = 'foobar'`. 
    '''

    def __init__(self, name, requires_db=False, accept_header=None, args=[]):
        self.name = name
        self.requires_db = requires_db
        self.args = args
        self.out = sys.stdout
        self.err = sys.stderr

        if accept_header is None:
            self.accept_header = self.accept('json')
        else:
            self.accept_header = accept_header

        self.passed = None
        self.failed = None
        self.warned = None

    def accept(self, content_type):
        return {
            'json': 'application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7',
            'html': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
            'plaintext': 'text/plain,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7'
        }[content_type]

    def setup_out(self, out):
        '''
        Sets up file-like objects for logging. Used in 
        cases where it is hard just return the output. Any
        output sent to these file objects is also printed to 
        the console

        NOTE: I detest this. It would be much better to use
        logging like it's intended
        '''
        self.out = out

    def parse(self, test_keys):
        '''
        Takes the dict of key/value pairs describing a FrameworkTest 
        and collects all variables needed by this FrameworkTestType

        Raises AttributeError if required keys are missing
        '''
        if all(arg in test_keys for arg in self.args):
            self.__dict__.update({arg: test_keys[arg] for arg in self.args})
            return self
        else:  # This is quite common - most tests don't support all types
            raise AttributeError(
                "A %s requires the benchmark_config.json to contain %s" % (self.name, self.args))

    def request_headers_and_body(self, url):
        '''
        Downloads a URL and returns the HTTP response headers
        and body content as a tuple
        '''
        print "Accessing URL %s:" % url
        self.out.write("Accessing URL %s \n" % url)

        headers = {'Accept': self.accept_header}
        r = requests.get(url, timeout=15, headers=headers)

        headers = r.headers
        body = r.content
        self.out.write(str(headers))
        self.out.write(body)
        b = 40
        print "  Response (trimmed to %d bytes): \"%s\"" % (b, body.strip()[:b])
        return headers, body

    def verify(self, base_url):
        '''
        Accesses URL used by this test type and checks the return 
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
        raise NotImplementedError("Subclasses must provide get_url")

    def copy(self):
        '''
        Returns a copy that can be safely modified.
        Use before calling parse
        '''
        return copy.copy(self)
