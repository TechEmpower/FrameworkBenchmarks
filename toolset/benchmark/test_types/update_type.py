from benchmark.test_types.framework_test_type import FrameworkTestType
from benchmark.test_types.query_type import QueryTestType

from pprint import pprint

class UpdateTestType(QueryTestType):
  def __init__(self):
    args = ['update_url']
    FrameworkTestType.__init__(self, name='update', requires_db=True, 
      accept_header=self.accept_json, args=args)

  def get_url(self):
    return self.update_url

  def verify(self, base_url):
    '''Validates the response is a JSON array of 
    the proper length, each JSON Object in the array 
    has keys 'id' and 'randomNumber', and these keys 
    map to integers. Case insensitive and 
    quoting style is ignored
    '''

    url = base_url + self.update_url
    problems = []
    
    body = self._curl(url + '2')
    problems += self._verifyQueryList(2, body, url + '2')

    body = self._curl(url + '0')
    problems += self._verifyQueryList(1, body, url + '0', 'warn')

    body = self._curl(url + 'foo')
    problems += self._verifyQueryList(1, body, url + 'foo', 'warn')

    body = self._curl(url + '501')
    problems += self._verifyQueryList(500, body, url + '501', 'warn')

    if len(problems) == 0:
      return [('pass','',url + '2'),
              ('pass','',url + '0'),
              ('pass','',url + 'foo'),
              ('pass','',url + '501')]
    else:
      return problems


