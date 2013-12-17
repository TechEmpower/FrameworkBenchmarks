from HTMLParser import HTMLParser

class FortuneHTMLParser(HTMLParser):
  body = []

  def handle_starttag(self, tag, attrs):
    print "start tag: '{s}'".format(s=tag)

  def handle_data (self, data):
    print "data: '{s}'".format(s=data)

  def handle_endtag(self, tag):
    print "end tag: '{s}'".format(s=tag)


  def isValidFortune(self):
    return True