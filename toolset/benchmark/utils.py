import ConfigParser
import os
import glob
import json
import socket

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

  # Old, hacky method to exclude all tests was to 
  # request a test known to not exist, such as ''. 
  # If test '' was requested, short-circuit and return 
  # nothing immediately
  if len(include) == 1 and '' in include:
    return []
    
  # Setup default Benchmarker using example configuration
  if benchmarker is None:
    print "Creating Benchmarker from benchmark.cfg.example"
    default_config = setup_util.get_fwroot() + "/benchmark.cfg.example"
    config = ConfigParser.SafeConfigParser()
    config.readfp(open(default_config))
    defaults = dict(config.items("Defaults"))
    
    # Convert strings into proper python types
    for k,v in defaults.iteritems():
      try:
        defaults[k] = literal_eval(v)
      except Exception:
        pass

    # Ensure we only run the __init__ method of Benchmarker
    defaults['install'] = None
    
    benchmarker = Benchmarker(defaults)

  
  # Search in both old and new directories
  fwroot = setup_util.get_fwroot() 
  config_files = glob.glob("%s/*/benchmark_config.json" % fwroot) 
  config_files.extend(glob.glob("%s/frameworks/*/*/benchmark_config.json" % fwroot))
  
  tests = []
  for config_file_name in config_files:
    config = None
    with open(config_file_name, 'r') as config_file:
      try:
        config = json.load(config_file)
      except ValueError:
        # User-friendly errors
        print("Error loading '%s'." % config_file_name)
        raise

    # Find all tests in the config file
    config_tests = framework_test.parse_config(config, 
      os.path.dirname(config_file_name), benchmarker)
        
    # Filter
    for test in config_tests:
      if len(include) is 0 and len(exclude) is 0:
        # No filters, we are running everything
        tests.append(test)
      elif test.name in exclude:
        continue
      elif test.name in include:
        tests.append(test)
      else: 
        # An include list exists, but this test is 
        # not listed there, so we ignore it
        pass

  # Ensure we were able to locate everything that was 
  # explicitly included 
  if 0 != len(include):
    names = {test.name for test in tests}
    if 0 != len(set(include) - set(names)):
      missing = list(set(include) - set(names))
      raise Exception("Unable to locate tests %s" % missing)

  tests.sort(key=lambda x: x.name)
  return tests

def gather_frameworks(include = [], exclude=[], benchmarker=None):
  '''Return a dictionary mapping frameworks->[test1,test2,test3]
  for quickly grabbing all tests in a grouped manner. 
  Args have the same meaning as gather_tests'''

  tests = gather_tests(include, exclude, benchmarker)
  frameworks = dict()

  for test in tests:
    if test.framework not in frameworks: 
      frameworks[test.framework] = []
    frameworks[test.framework].append(test)
  return frameworks

def header(message, top='-', bottom='-'):
    '''
    Generates a clean header
    '''
    topheader = (top * 80)[:80]
    bottomheader = (bottom * 80)[:80]
    result = ""
    if topheader != "":
      result += "%s" % topheader
    if message != "":
      if result == "":
        result = "  %s" % message
      else:
        result += "\n  %s" % message
    if bottomheader != "":
      if result == "":
        result = "%s" % bottomheader
      else:
        result += "\n%s" % bottomheader
    return result + '\n'

def check_services(services):

  def check_service(address, port):
    try:
      s = socket.socket()
      s.settimeout(20)
      s.connect((address, port))
      return (True, "")
    except Exception as ex:
      return (False, ex)
    finally:
      s.close

  res = []
  for s in services:
    r = check_service(s[1], s[2])
    res.append((s[0], r[0], str(r[1])))
  return res

def verify_database_connections(services):
  allGo = True
  messages = []
  for r in check_services(services):
    if r[1]:
      messages.append(r[0] + ": is GO!")
    else:
      messages.append(r[0] + ": is _NO_ GO!: ERROR: " + r[2])
      allGo = False
  return (allGo, messages)
