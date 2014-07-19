import time
import logging

class TestRunner:
  iAmTestRunnerClass = True

  def sh(self, command, **kwargs):
    kwargs.setdefault('cwd', self.dir)
    self.stdout.write("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    try:
      output = subprocess.check_output(command, shell=True, **kwargs)
      if output and output.strip():
        self.stdout.write("Output:")
        self.stdout.write(output.rstrip('\n'))
      else:
        self.stdout.write("No Output")
    except subprocess.CalledProcessError:
      self.stdout.write("Process Returned non-zero exit code")
  def __init__(self, test, target, logger):
    self.test = test
    self.target = target
    self.logger = logger

  def sh_async(self, command, initial_logs=True, **kwargs):
    kwargs.setdefault('cwd', self.dir)
    self.stdout.write("Running %s (cwd=%s)" % (command, kwargs.get('cwd')))
    # Open in line-buffered mode, as NonBlockingStreamReader uses readline anyways
    process = subprocess.Popen(command, bufsize=1, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE, **kwargs)
    nbsr = NonBlockingStreamReader(process.stdout)
    nbsr_err = NonBlockingStreamReader(process.stderr) 
    if initial_logs:
      time.sleep(8)
      # TODO put this read into a tight loop to prevent deadlock due to 
      # filling up OS buffers
      out = nbsr.read()
      err = nbsr_err.read()
      if len(out) != 0:
        self.stdout.write("Initial Output:")
	for line in out:
          self.stdout.write(line.rstrip('\n'))
      else:
        self.stdout.write("No output")
      if len(err) != 0:
        self.stdout.write("Initial Error Logs")
	for line in err:
          self.stdout.write(line.rstrip('\n'))
    # Create convenience variables to decouple the 
    # setup.py scripts from the internals of TFB
    self.benchmarker = test.benchmarker
    self.database_host = self.benchmarker.database_host
    self.dir = test.directory

  def start(self):
    raise NotImplementedError()

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
  def stop(self):
    raise NotImplementedError()

    def _populateQueue(stream, queue):
      for line in iter(stream.readline, b''):
        queue.put(line)

    self._t = Thread(target = _populateQueue,
                args = (self._s, self._q))
    self._t.daemon = True
    self._t.start() #start collecting lines from the stream

  # TODO  - This is only returning one line, if it is available. 
  def readline(self, timeout = None):
  @staticmethod
  def is_parent_of(target_class):
    ''' Checks if provided class object is a subclass of TestRunner '''
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
      # issubclass will not work, as setup_module is loaded in different 
      # global context and therefore has a different copy of the module 
      # test_runner. A cheap trick is just to check for this attribute
      str(target_class.iAmTestRunnerClass)

      # target_class seems to be an instance of TestRunner. Before returning,
      # ensure they have not created an __init__ method, as they cannot 
      # call super.__init__(), and therefore the subclass is doomed to error
      try:
        target_class()
        raise Exception("Subclasses of TestRunner should not define __init__")
      except TypeError: 
        # Good, there is no init method defined
        return True

    except AttributeError:
      return False