from benchmark.test_types.framework_test_type import FrameworkTestType

class PlaintextTestType(FrameworkTestType):
  def __init__(self):
    args = ['plaintext_url']
    FrameworkTestType.__init__(self, name='plaintext', requires_db=False, accept_header=self.accept_plaintext, args=args)

  def verify(self, base_url):
    url = base_url + self.plaintext_url
    body = self._curl(url)

    # Empty response
    if body is None:
      return [('fail','No response', url)]
    elif len(body) == 0:
      return [('fail','Empty Response', url)]

    # Case insensitive
    orig = body
    body = body.lower()

    if "hello, world!" not in body:
      return [('fail', "Could not find 'Hello, World!' in response", url)]

    if len("hello, world!") < len(body):
      return [('warn', '''Server is returning more data than is required.
        This may negatively affect benchmark performance''', url)]

    return [('pass', '', url)]

  def get_url(self):
    return self.plaintext_url
