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


