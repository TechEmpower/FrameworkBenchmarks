import tempfile


class WrapLogger():
  """
  Used to convert a Logger into file streams. Adds easy integration 
  of Logger into subprocess, which takes file parameters for stdout
  and stderr. 

  Use: 
    (out, err) = WrapLogger(logger, logging.INFO), WrapLogger(logger, logging.ERROR)
    subprocess.Popen(command, stdout=out, stderr=err)
  """
  def __init__(self, logger, level):
    self.logger = logger
    self.level = level
    self.file = tempfile.TemporaryFile()

  def write(self, message):
    self.logger.log(self.level, message)

  def __getattr__(self, name):
    return getattr(self.file, name)


class Header():
  """
  """

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

