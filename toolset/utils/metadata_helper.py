import ConfigParser
import os
import glob
import json

from ast import literal_eval


def gather_langauges():
  '''
  Gathers all the known languages in the suite via the folder names
  beneath FWROOT.
  '''
  # Avoid setting up a circular import
  from setup.linux import setup_util

  lang_dir = os.path.join(setup_util.get_fwroot(), "frameworks")
  langs = []
  for dir in glob.glob(os.path.join(lang_dir, "*")):
    langs.append(dir.replace(lang_dir,"")[1:])
  return langs


def gather_tests(include = [], exclude=[], benchmarker_config=None, results=None):
  '''
  Given test names as strings, returns a list of FrameworkTest objects.
  For example, 'aspnet-mysql-raw' turns into a FrameworkTest object with
  variables for checking the test directory, the test database os, and
  other useful items.

  With no arguments, every test in this framework will be returned.
  With include, only tests with this exact name will be returned.
  With exclude, all tests but those excluded will be returned.

  A config is needed to construct full FrameworkTest objects. If
  one is not provided, a default config will be created.
  '''

  # Avoid setting up a circular import
  from benchmark import framework_test
  from BenchmarkConfig import BenchmarkConfig
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

  # Setup default BenchmarkerConfig using example configuration
  if benchmarker_config is None:
    default_config = setup_util.get_fwroot() + "/benchmark.cfg"
    config = ConfigParser.SafeConfigParser()
    config.readfp(open(default_config))
    defaults = dict(config.items("Defaults"))

    # Convert strings into proper python types
    for k,v in defaults.iteritems():
      try:
        defaults[k] = literal_eval(v)
      except Exception:
        pass

    defaults['results_name'] = "(unspecified, datetime = %Y-%m-%d %H:%M:%S)"
    defaults['results_environment'] = "My Server Environment"
    defaults['test_dir'] = None
    defaults['test_lang'] = None
    defaults['quiet'] = True

    benchmarker_config = BenchmarkConfig(defaults)

  # Search for configuration files
  config_files = []

  if benchmarker_config.test_lang:
    benchmarker_config.test_dir = []
    for lang in benchmarker_config.test_lang:
      if os.path.exists("{!s}/frameworks/{!s}".format(benchmarker_config.fwroot, lang)):
        for test_dir in os.listdir("{!s}/frameworks/{!s}".format(benchmarker_config.fwroot, lang)):
          benchmarker_config.test_dir.append("{!s}/{!s}".format(lang, test_dir))
      else:
        raise Exception("Unable to locate language directory: {!s}".format(lang))
  elif benchmarker_config.test_dir:
    for test_dir in benchmarker_config.test_dir:
      dir_config_files = glob.glob("{!s}/frameworks/{!s}/benchmark_config.json".format(benchmarker_config.fwroot, test_dir))
      if len(dir_config_files):
        config_files.extend(dir_config_files)
      else:
        raise Exception("Unable to locate tests in test-dir: {!s}".format(test_dir))
  elif benchmarker_config.test:
    for test in benchmarker_config.test:
      dir_config_files = glob.glob("{!s}/frameworks/*/{!s}/benchmark_config.json".format(benchmarker_config.fwroot, test))
      if len(dir_config_files):
        config_files.extend(dir_config_files)
      else:
        raise Exception("Unable to locate tests in test: {!s}".format(benchmarker_config.test))
  else:
    config_files.extend(glob.glob("{!s}/frameworks/*/*/benchmark_config.json".format(benchmarker_config.fwroot)))

  tests = []
  for config_file_name in config_files:
    config = None
    with open(config_file_name, 'r') as config_file:
      try:
        config = json.load(config_file)
      except ValueError:
        raise Exception("Error loading '{!s}'.".format(config_file_name))

  # Find all tests in the config file
  config_tests = framework_test.parse_config(config,
    os.path.dirname(config_file_name), benchmarker_config, results)

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
    

def gather_remaining_tests(config, results):
  '''
  Gathers the tests remaining in a current benchmark run.
  '''
  tests = gather_tests(config.test, config.exclude, config, results)

  # If the tests have been interrupted somehow, then we want to resume them where we left
  # off, rather than starting from the beginning
  if os.path.isfile(config.current_benchmark):
    with open(config.current_benchmark, 'r') as interrupted_benchmark:
      interrupt_bench = interrupted_benchmark.read().strip()
    for index, atest in enumerate(tests):
      if atest.name == interrupt_bench:
        tests = tests[index:]
        break
  return tests


def gather_frameworks(include = [], exclude=[], config=None):
  '''Return a dictionary mapping frameworks->[test1,test2,test3]
  for quickly grabbing all tests in a grouped manner.
  Args have the same meaning as gather_tests'''

  tests = gather_tests(include, exclude, config)
  frameworks = dict()

  for test in tests:
    if test.framework not in frameworks:
      frameworks[test.framework] = []
    frameworks[test.framework].append(test)
  return frameworks