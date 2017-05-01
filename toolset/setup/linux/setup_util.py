import re
import os
import sys
import subprocess
import platform

from threading import Thread
from Queue import Queue, Empty

class NonBlockingStreamReader:
  '''
  Enables calling readline in a non-blocking manner with a blocking stream, 
  such as the ones returned from subprocess.Popen

  Originally written by Eyal Arubas, who granted permission to use this inside TFB
  See http://eyalarubas.com/python-subproc-nonblock.html
  '''
  def __init__(self, stream, eof_message = None):
    '''
    stream: the stream to read from.
            Usually a process' stdout or stderr.
    eof_message: A message to print to stdout as soon
      as the stream's end is reached. Useful if you
      want to track the exact moment a stream terminates
    '''

    self._s = stream
    self._q = Queue()
    self._eof_message = eof_message
    self._poisonpill = 'MAGIC_POISONPILL_STRING'

    def _populateQueue(stream, queue):
      while True:
        line = stream.readline()
        if line: # 'data\n' or '\n'
          queue.put(line)
        else:    # '' e.g. EOF
          if self._eof_message:
            sys.stdout.write(self._eof_message + '\n')
          queue.put(self._poisonpill)
          return

    self._t = Thread(target = _populateQueue,
            args = (self._s, self._q))
    self._t.daemon = True
    self._t.start()

  def readline(self, timeout = None):
    try:
      line = self._q.get(block = timeout is not None,
        timeout = timeout)
      if line == self._poisonpill: 
        raise EndOfStream
      return line
    except Empty:
      return None

class EndOfStream(Exception): pass

# Replaces all text found using the regular expression to_replace with the supplied replacement.
def replace_text(file, to_replace, replacement):
    with open(file, "r") as conf:
        contents = conf.read()
    replaced_text = re.sub(to_replace, replacement, contents)
    with open(file, "w") as f:
        f.write(replaced_text)

# Queries the shell for the value of FWROOT
def get_fwroot():

    if os.getenv('FWROOT'):
        return os.environ['FWROOT']
    else:
        return os.getcwd()

# Turns absolute path into path relative to FWROOT
# Assumes path is underneath FWROOT, not above
# 
# Useful for clean presentation of paths 
# e.g. /foo/bar/benchmarks/go/install.sh
# v.s. FWROOT/go/install.sh 
def path_relative_to_root(path):
    # Requires bash shell parameter expansion
    return subprocess.check_output("D=%s && printf \"${D#%s}\""%(path, get_fwroot()), shell=True, executable='/bin/bash')
