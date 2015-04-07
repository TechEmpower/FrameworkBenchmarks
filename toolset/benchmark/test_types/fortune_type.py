from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.fortune_html_parser import FortuneHTMLParser

class FortuneTestType(FrameworkTestType):
  def __init__(self):
    args = ['fortune_url']
    FrameworkTestType.__init__(self, name='fortune', 
      requires_db=True, 
      accept_header=self.accept_html, 
      args=args)

  def get_url(self):
    return self.fortune_url

  def verify(self, base_url):
    '''Parses the given HTML string and asks the 
    FortuneHTMLParser whether the parsed string is a 
    valid fortune response
    '''
    url = base_url + self.fortune_url
    full_response = self._curl(url)
    body = self._curl_body(url)
    
    # Empty response
    if body is None:
      return [('fail','No response', url)]
    elif len(body) == 0:
      return [('fail','Empty Response', url)]

    parser = FortuneHTMLParser()
    parser.feed(body)
    (valid, diff) = parser.isValidFortune(self.out)
    if valid:
      # Ensure required response headers are present
      if any(v.lower() not in full_response.lower() for v in ('Server','Date','Content-Type: text/html')) \
         or all(v.lower() not in full_response.lower() for v in ('Content-Length','Transfer-Encoding')):
        return[('warn','Required response header missing.',url)]

      return [('pass','',url)]
    else:
      failures = [('fail','Invalid according to FortuneHTMLParser',url)]
      # Catch exceptions because we are relying on internal code
      try:
        # Parsing this: 
        # --- Valid
        # +++ Response
        # @@ -1 +1 @@
        #
        # -<!doctype html><html><head><title>Fortunes</title></head><body><table>
        # +<!doctype html><html><head><meta></meta><title>Fortunes</title></head><body><div><table>
        # @@ -16 +16 @@
        
        current_neg = []
        current_pos = []
        for line in diff[3:]:
          if line[0] == '+':
            current_neg.append(line[1:])
          elif line[0] == '-':
            current_pos.append(line[1:])
          elif line[0] == '@':
            failures.append( ('fail', 
              "`%s` should be `%s`" % (''.join(current_neg), ''.join(current_pos)),
              url) )
        if len(current_pos) != 0:
          failures.append( ('fail', 
              "`%s` should be `%s`" % (''.join(current_neg), ''.join(current_pos)),
              url) )
      except: 
        pass
      return failures 

