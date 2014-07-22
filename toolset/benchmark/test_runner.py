import time
import logging
from utils import WrapLogger
from utils import ShellUtils

class TestRunner:
  iAmTestRunnerClass = True

  def __init__(self, test, target, logger):
    self.test = test
    self.target = target
    self.logger = logger

    # Create convenience variables to decouple the 
    # setup.py scripts from the internals of TFB
    self.benchmarker = test.benchmarker
    self.database_host = self.benchmarker.database_host
    self.dir = test.directory
    
    (out, err) = WrapLogger(logger, logging.INFO), WrapLogger(logger, logging.ERROR)
    self.utils = ShellUtils(self.dir, None, None, logger)

  def start(self):
    raise NotImplementedError()

  def stop(self):
    raise NotImplementedError()

  def sh(self, *args, **kwargs):
    self.utils.sh(*args, **kwargs)

  def sh_async(self, *args, **kwargs):
    return self.utils.sh_async(*args, **kwargs)

  def sh_pkill(self, *args, **kwargs):
    self.logger.debug("Got %s and %s" % (args, kwargs))
    self.utils.sh_pkill(*args, **kwargs)

  @staticmethod
  def is_parent_of(target_class):
    ''' Checks if provided class object is a subclass of TestRunner '''
    try:
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