
class Header():
  '''
  Generates a clean header
  '''

  def __init__(self, message, top='-', bottom='-'):
    self.message = message
    self.top = top
    self.bottom = bottom

  def __str__(self):
    topheader = self.top * 80
    topheader = topheader[:80]
    bottomheader = self.bottom * 80
    bottomheader = bottomheader[:80]
    return "%s\n  %s\n%s" % (topheader, self.message, bottomheader)

