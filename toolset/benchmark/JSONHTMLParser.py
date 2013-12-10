from HTMLParser import HTMLParser

# HTMLParser which treats all tags as superfluous
# markup, and appends every bit of data to an object
# that gets returned later.
class JSONHTMLParser(HTMLParser):
  # We are going to append out data to this.
  body = []

  # Every instance of data inside of an html node
  # will cause this function to be called.
  def handle_data (self, data):
    self.body.append(data)

  # After a parse of an html document, this method
  # will get the body parsed out as a string.
  def getBody(self):
    return str(self.body);