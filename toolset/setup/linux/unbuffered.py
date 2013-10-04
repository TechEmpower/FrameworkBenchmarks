# Wrapper for unbuffered stream writing.
# http://stackoverflow.com/a/107717/376366
# Used to make sure print output appears in the correct order
# in log files when spawning subprocesses.

class Unbuffered:
  def __init__(self, stream):
    self.stream = stream
  def write(self, data):
    self.stream.write(data)
    self.stream.flush()
  def __getattr__(self, attr):
    return getattr(self.stream, attr)
