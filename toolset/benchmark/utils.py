import tempfile

import subprocess
import time
class ShellUtils():
  def __init__(self, directory, outfile, errfile):
    '''
    outfile: A file-like object to write command output to. 
             Must have write(string) method. Common choices are 
             files, sys.stdout, or WrapLogger objects
    errfile: See outfile 
    '''
    # Advanced notes: outfile and errfile do *not* have to be 
    # thread-safe objects. They are only ever written to from one 
    # thread at a time *unless* someone calls sh_async twice with 
    # the same ShellUtils
    self.directory = directory
    self.outfile = outfile
    self.errfile = errfile
  
  def sh(self, command, **kwargs):
    '''Run a shell command, sending output to outfile and errfile.
    Blocks until command exits'''
    kwargs.setdefault('cwd', self.directory)
    kwargs.setdefault('executable', '/bin/bash')
    self.outfile.write("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    try:
      output = subprocess.check_output(command, shell=True, stderr=self.errfile, **kwargs)
      if output and output.strip():
        self.outfile.write("Output:")
        self.outfile.write(output.rstrip('\n'))
      else:
        self.outfile.write("No Output")
    except subprocess.CalledProcessError:
      self.errfile.write("Command returned non-zero exit code: %s" % command)

  # TODO modify this to start the subcommand as a new process group, so that 
  # we can automatically kill the entire group!
  def sh_async(self, command, initial_logs=True, **kwargs):
    '''Run a shell command, sending output to outfile and errfile.
    If intial_logs, prints out logs for a few seconds before returning. '''
    # TODO add this - '''Continues to send output until command completes'''
    kwargs.setdefault('cwd', self.directory)
    self.outfile.write("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    
    # Open in line-buffered mode (bufsize=1) because NonBlockingStreamReader uses readline anyway
    process = subprocess.Popen(command, bufsize=1, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE, **kwargs)
    nbsr = NonBlockingStreamReader(process.stdout)
    nbsr_err = NonBlockingStreamReader(process.stderr) 
    if initial_logs:
      time.sleep(8)
      # TODO put this read into a tight loop to prevent deadlock due to 
      # filling up OS buffers
      out = nbsr.read()
      if len(out) == 0:
        self.outfile.write("No output")
      else: 
        self.outfile.write("Initial Output:")
        for line in out:
            self.outfile.write(line.rstrip('\n'))
        
      err = nbsr_err.read()
      if len(err) != 0:
        self.errfile.write("Initial Error Logs:")
        for line in err:
          self.errfile.write(line.rstrip('\n'))

from threading import Thread
from Queue import Queue, Empty

# TODO - no need to use a daemon, kill this off in stop!
# NOTE - it is safe to use logging module in a multi-threaded
# system, but not safe to log to the same file across multiple 
# processes. Our system has two main processes (main and __run_test), 
# and lots of minor ones from 'subprocess'. As long as we only use
# one logger inside TestRunner and NonBlockingFoo, sd are good
# Add credit for http://eyalarubas.com/python-subproc-nonblock.html
class NonBlockingStreamReader:
  def __init__(self, stream):
    self._s = stream
    self._q = Queue()

    def _populateQueue(stream, queue):
      for line in iter(stream.readline, b''):
        queue.put(line)

    self._t = Thread(target = _populateQueue,
                args = (self._s, self._q))
    self._t.daemon = True
    self._t.start() #start collecting lines from the stream

  # TODO  - This is only returning one line, if it is available. 
  def readline(self, timeout = None):
    try:
      return self._q.get(block = timeout is not None,
                 timeout = timeout)
    except Empty:
      return None
  
  def read(self):
    lines = []
    while True:
      line = self.readline(0.1)
      if not line:
        return lines
      lines.append(line)
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

  def __del__(self):
    """Grabs any output that was written directly to the file (e.g. bypassing 
    the write method). Subprocess.call, Popen, etc have a habit of accessing 
    the file directly for faster writing. See http://bugs.python.org/issue1631
    """
    self.file.seek(0)
    for line in self.file.readlines():
      self.logger.log(self.level, line.rstrip('\n'))

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
    return "\n%s\n  %s\n%s" % (topheader, self.message, bottomheader)

