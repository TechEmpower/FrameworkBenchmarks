from benchmark.test_types.framework_test_type import FrameworkTestType

class PlaintextTestType(FrameworkTestType):
  def __init__(self):
    args = ['plaintext_url']
    FrameworkTestType.__init__(self, name='plaintext', requires_db=False, accept_header=self.accept_plaintext, args=args)

  def verify(self, base_url):
    url = base_url + self.plaintext_url
    full_response = self._curl(url)
    body = self._curl_body(url)

    # Empty response
    if body is None:
      return [('fail','No response', url)]
    elif len(body) == 0:
      return [('fail','Empty Response', url)]

    # Case insensitive
    orig = body
    body = body.lower()

    if "hello, world!" not in body:
      return [('fail', """Could not find 'Hello, World!' in response.""", url)]

    if len("hello, world!") < len(body):
      return [('warn', """Server is returning %s more bytes than are required.
This may negatively affect benchmark performance.""" % (len(body) - len("hello, world!")), url)]

    # Ensure required response headers are present
    if any(v.lower() not in full_response.lower() for v in ('Server','Date','Content-Type: text/plain')) \
       or all(v.lower() not in full_response.lower() for v in ('Content-Length','Transfer-Encoding')):
      return [('warn','Required response header missing.',url)]

    return [('pass', '', url)]

  def get_url(self):
    return self.plaintext_url
