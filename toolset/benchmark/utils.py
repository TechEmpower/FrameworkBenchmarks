import ConfigParser
import os
import glob
import json

from ast import literal_eval

def gather_tests(include = [], exclude=[], benchmarker=None):
  '''
  Given test names as strings, returns a list of FrameworkTest objects. 
  For example, 'aspnet-mysql-raw' turns into a FrameworkTest object with
  variables for checking the test directory, the test database os, and 
  other useful items. 

  With no arguments, every test in this framework will be returned.  
  With include, only tests with this exact name will be returned. 
  With exclude, all tests but those excluded will be returned. 

  A benchmarker is needed to construct full FrameworkTest objects. If
  one is not provided, a default Benchmarker will be created. 
  '''

  # Avoid setting up a circular import
  from benchmark import framework_test
  from benchmark.benchmarker import Benchmarker
  from setup.linux import setup_util

  # Help callers out a bit
  if include is None:
    include = []
  if exclude is None:
    exclude = []
  
  # Setup default Benchmarker using example configuration
  if benchmarker is None:
    default_config = setup_util.get_fwroot() + "/benchmark.cfg.example"
    config = ConfigParser.SafeConfigParser()
    config.readfp(open(default_config))
    defaults = dict(config.items("Defaults"))
    
    # Convert strings into proper python types
    for k,v in defaults.iteritems():
      try:
        defaults[k] = literal_eval(v)
      except:
        pass

    # Ensure we only run the __init__ method of Benchmarker
    defaults['install'] = None
    
    benchmarker = Benchmarker(defaults)

  
  # Search in both old and new directories
  fwroot = os.environ['FWROOT']
  config_files = glob.glob("%s/*/benchmark_config" % fwroot) 
  config_files.extend(glob.glob("%s/frameworks/*/*/benchmark_config" % fwroot))
  
  tests = []
  for config_file_name in config_files:
    config = None
    with open(config_file_name, 'r') as config_file:
      try:
        config = json.load(config_file)
      except:
        # User-friendly errors
        print("Error loading '%s'." % config_file_name)
        raise

    # Find all tests in the config file
    config_tests = framework_test.parse_config(config, 
      os.path.dirname(config_file_name), benchmarker)
    
    # Filter
    for test in config_tests:
      if test.name in exclude:
        continue
      elif len(include) is 0 or test.name in include:
        tests.append(test)

  tests.sort(key=lambda x: x.name)
  return tests

def header(message, top='-', bottom='-'):
    '''
    Generates a clean header
    '''
    topheader = (top * 80)[:80]
    bottomheader = (bottom * 80)[:80]
    return "%s\n  %s\n%s" % (topheader, message, bottomheader)
