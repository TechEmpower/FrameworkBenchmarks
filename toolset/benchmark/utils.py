
import subprocess
import time
import logging
import shlex

class ShellUtils():
  def __init__(self, directory, outfile, errfile, logger=None):
    '''
    outfile: A file-like object to write command output to. 
             Must have write(string) method. Common choices are 
             files, sys.stdout, or WrapLogger objects
    errfile: See outfile 
    logger : If provided, used instead of outfile/errfile for 
             finer-grained logging
    '''
    # Advanced notes: outfile and errfile do *not* have to be 
    # thread-safe objects. They are only ever written to from one 
    # thread at a time *unless* someone calls sh_async twice with 
    # the same ShellUtils
    self.directory = directory
    self.outfile = outfile
    self.errfile = errfile
    self.logger = logger
  
  def __write_out(self, message, level=logging.INFO, stream=None):
    if self.logger:
      self.logger.log(level, message)
    elif stream == None:
      self.outfile.write(message)
    else:
      stream.write(message)
  
  def __write_err(self, message, level=logging.ERROR):
    self.__write_out(message, level, stream=self.errfile)
  
  def sh(self, command, **kwargs):
    '''Run a shell command, sending output to outfile and errfile.
    Blocks until command exits'''
    kwargs.setdefault('cwd', self.directory)
    kwargs.setdefault('executable', '/bin/bash')
    self.__write_out("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    try:
      output = subprocess.check_output(command, shell=True, stderr=self.errfile, **kwargs)
      if output and output.strip():
        self.__write_out("Output:")
        self.__write_out(output.rstrip('\n'))
      else:
        self.__write_out("No Output")
    except subprocess.CalledProcessError:
      self.__write_err("Command returned non-zero exit code: %s" % command)

  def sh_async(self, command, group=True, **kwargs):
    '''
    Run a shell command, continually sending output to outfile and errfile until 
    shell process completes

    - If group is set, create a process group that can later be used to kill all subprocesses
    Returns the pid of the newly created process (or process group)
    '''
    
    # Setup extra args
    kwargs.setdefault('cwd', self.directory)
    if group:
      if self.os != 'nt':
        kwargs.setdefault('preexec_fn', os.setpgrp)
      else:
        # TODO if someone could make this work that would be great
        self.__write_err("Unable to group flag on Windows")
    
    self.__write_out("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    # Open in line-buffered mode (bufsize=1) because NonBlocking* uses readline
    process = subprocess.Popen(command, bufsize=1, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE, **kwargs)
    NonBlockingStreamLogger(process, self.logger, name=shlex.split(command)[0])
    return process.pid

  def sh_pkill(self, group_id, name=None, usesudo=False):
    '''
    Kill processes that match all the passed arguments
    Set group_id if you used sh_async
    Set usesudo only if you started these processes with sudo
    # TODO - consider os.pgkill?
    '''
    command = "pkill "
    command = "%s -g %s" % (command, group_id)

    if name:
      command = "%s %s" % (command, name)
    if usesudo:
      command = "sudo %s" % command
    self.sh(command)


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

import logging
from threading import Thread
import threading
# NonBlockingStreamLogger(p, logging.getLogger())
# p = subprocess.Popen("asdfasdf", shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
class NonBlockingStreamLogger:
  '''Reads from a subprocess' streams and writes them to the 
  provided logger as long as the subprocess is alive'''
  def __init__(self, process, logger, logout=logging.INFO, logerr=logging.ERROR, name=None):
    self.process = process
    self.logger = logger
    self.out_level = logout
    self.err_level = logerr
    if name:
      self.prefix = "Process '%s':" % name
    else: 
      self.logger.warning("No name provided for process %s", process.pid)
      self.prefix = "Process '%s':" % process.pid
      name = process.pid
    outThread = Thread(target = self._readStream,
                args = (process.stdout, self.out_level),
                name = "%s - stdout" % name)
    outThread.daemon = True
    outThread.start()
    errThread = Thread(target = self._readStream,
                args = (process.stderr, self.err_level),
                name = "%s - stderr" % name)
    errThread.daemon = True
    errThread.start()
  def _readStream(self, stream, level):
    self.logger.debug("%s Waiting for output (%s)", self.prefix, threading.current_thread().name)
    for line in iter(stream.readline, b''):
      self.logger.log(level, "%s %s", self.prefix, line.rstrip('\n'))
      
      # Has process died? 
      if self.process.poll() == 0:
        self.logger.debug("%s Death. Reading remainder of stream", self.prefix)
        remainder = stream.read()
        for line2 in remainder.split(b'\n'):
          self.logger.log(level, "%s %s", self.prefix, line2)
        break
    self.logger.debug("%s Complete (%s)", self.prefix, threading.current_thread().name)
    return 0
import tempfile
class WrapLogger():
  """
  Used to convert a Logger into a file-like object. Adds easy integration 
  of Logger into subprocess, which takes file parameters for stdout
  and stderr. 

  Use: 
    (out, err) = WrapLogger(logger, logging.INFO), WrapLogger(logger, logging.ERROR)
    subprocess.Popen(command, stdout=out, stderr=err)

  Note: When used with subprocess, this cannot guarantee that output will appear
  in real time. This is because subprocess tends to bypass the write() method and 
  access the underlying file directly. This will eventually collect any output
  that was sent directly to the file, but it cannot do this in real time. 
  Practically, this limitation means that WrapLogger is safe to use with 
  all synchronous subprocess calls, but it will lag heavily with 
  subprocess.Popen calls
  """
  # Note - Someone awesome with python could make this fully implement the file 
  # interface, and remove the real-time limitation
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

