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
    body = self._curl(url)
    
    # Empty response
    if body is None:
      return [('fail','No response', url)]
    elif len(body) == 0:
      return [('fail','Empty Response', url)]

    parser = FortuneHTMLParser()
    parser.feed(body)
    if parser.isValidFortune(self.out):
      return [('pass','',url)]
    else:
      return [('fail','Invalid according to FortuneHTMLParser',url)]

