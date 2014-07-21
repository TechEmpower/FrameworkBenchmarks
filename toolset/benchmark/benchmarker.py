from setup.linux.installer import Installer
from setup.linux import setup_util

from benchmark import framework_test
from utils import WrapLogger
from utils import Header

import os
import json
import subprocess
import time
import pprint
import csv
import sys
import logging
log = logging.getLogger('benchmarker')
import socket
import glob
from multiprocessing import Process
from datetime import datetime

class Benchmarker:

  ##########################################################################################
  # Public methods
  ##########################################################################################

  ############################################################
  # Prints all the available tests
  ############################################################
  def run_list_tests(self):
    all_tests = self.__gather_tests

    for test in all_tests:
      print test.name

    self.__finish()
  ############################################################
  # End run_list_tests
  ############################################################

  ############################################################
  # Prints the metadata for all the available tests
  ############################################################
  def run_list_test_metadata(self):
    all_tests = self.__gather_tests
    all_tests_json = json.dumps(map(lambda test: {
      "name": test.name,
      "approach": test.approach,
      "classification": test.classification,
      "database": test.database,
      "framework": test.framework,
      "language": test.language,
      "orm": test.orm,
      "platform": test.platform,
      "webserver": test.webserver,
      "os": test.os,
      "database_os": test.database_os,
      "display_name": test.display_name,
      "notes": test.notes,
      "versus": test.versus
    }, all_tests))

    with open(os.path.join(self.full_results_directory(), "test_metadata.json"), "w") as f:
      f.write(all_tests_json)

    self.__finish()


  ############################################################
  # End run_list_test_metadata
  ############################################################
  
  ############################################################
  # parse_timestamp
  # Re-parses the raw data for a given timestamp
  ############################################################
  def parse_timestamp(self):
    log.info("parse_timestamp")
    all_tests = self.__gather_tests

    for test in all_tests:
      test.parse_all()
    
    self.__parse_results(all_tests)

    self.__finish()

  ############################################################
  # End parse_timestamp
  ############################################################

  ############################################################
  # Run the tests:
  # This process involves setting up the client/server machines
  # with any necessary change. Then going through each test,
  # running their setup script, verifying the URLs, and
  # running benchmarks against them.
  ############################################################
  def run(self):
    log.info("run")

    ##########################
    # Get a list of all known
    # tests that we can run.
    ##########################    
    all_tests = self.__gather_tests

    ##########################
    # Setup client/server
    ##########################
    print Header("Preparing Server, Database, and Client ...", top='=', bottom='=')
    self.__setup_server()
    self.__setup_database()
    self.__setup_client()

    ## Check if wrk (and wrk-pipeline) is installed and executable, if not, raise an exception
    #if not (os.access("/usr/local/bin/wrk", os.X_OK) and os.access("/usr/local/bin/wrk-pipeline", os.X_OK)):
    #  raise Exception("wrk and/or wrk-pipeline are not properly installed. Not running tests.")

    ##########################
    # Run tests
    ##########################
    print Header("Running Tests...", top='=', bottom='=')
    self.__run_tests(all_tests)

    ##########################
    # Parse results
    ##########################  
    if self.mode == "benchmark":
      print Header("Parsing Results ...", top='=', bottom='=')
      self.__parse_results(all_tests)

    self.__finish()

  ############################################################
  # End run
  ############################################################

  ############################################################
  # database_sftp_string(batch_file)
  # generates a fully qualified URL for sftp to database
  ############################################################
  def database_sftp_string(self, batch_file):
    sftp_string =  "sftp -oStrictHostKeyChecking=no "
    if batch_file != None: sftp_string += " -b " + batch_file + " "

    if self.database_identity_file != None:
      sftp_string += " -i " + self.database_identity_file + " "

    return sftp_string + self.database_user + "@" + self.database_host
  ############################################################
  # End database_sftp_string
  ############################################################

  ############################################################
  # client_sftp_string(batch_file)
  # generates a fully qualified URL for sftp to client
  ############################################################
  def client_sftp_string(self, batch_file):
    sftp_string =  "sftp -oStrictHostKeyChecking=no "
    if batch_file != None: sftp_string += " -b " + batch_file + " "

    if self.client_identity_file != None:
      sftp_string += " -i " + self.client_identity_file + " "

    return sftp_string + self.client_user + "@" + self.client_host
  ############################################################
  # End client_sftp_string
  ############################################################

  ############################################################
  # generate_url(url, port)
  # generates a fully qualified URL for accessing a test url
  ############################################################
  def generate_url(self, url, port):
    return self.server_host + ":" + str(port) + url
  ############################################################
  # End generate_url
  ############################################################

  ############################################################
  # get_output_file(test_name, test_type)
  # returns the output file name for this test_name and 
  # test_type timestamp/test_type/test_name/raw 
  ############################################################
  def get_output_file(self, test_name, test_type):
    log.debug("get_output_file")
    return os.path.join(self.result_directory, self.timestamp, test_type, test_name, "raw")
  ############################################################
  # End get_output_file
  ############################################################

  ############################################################
  # output_file(test_name, test_type)
  # returns the output file for this test_name and test_type
  # timestamp/test_type/test_name/raw 
  ############################################################
  def output_file(self, test_name, test_type):
    log.debug("output_file")
    path = self.get_output_file(test_name, test_type)
    try:
      os.makedirs(os.path.dirname(path))
    except OSError:
      pass
    return path
  ############################################################
  # End output_file
  ############################################################

  ############################################################
  # get_warning_file(test_name, test_type)
  # returns the output file name for this test_name and 
  # test_type timestamp/test_type/test_name/raw 
  ############################################################
  def get_warning_file(self, test_name, test_type):
    return os.path.join(self.result_directory, self.timestamp, test_type, test_name, "warn")
  ############################################################
  # End get_warning_file
  ############################################################

  ############################################################
  # warning_file(test_name, test_type)
  # returns the warning file for this test_name and test_type
  # timestamp/test_type/test_name/raw 
  ############################################################
  def warning_file(self, test_name, test_type):
    path = self.get_warning_file(test_name, test_type)
    try:
      os.makedirs(os.path.dirname(path))
    except OSError:
      pass
    return path
  ############################################################
  # End warning_file
  ############################################################

  ############################################################
  # full_results_directory
  ############################################################
  def full_results_directory(self):
    log.debug("full_results_directory")
    path = os.path.join(self.result_directory, self.timestamp)
    try:
      os.makedirs(path)
    except OSError:
      pass
    return path
  ############################################################
  # End full_results_directory
  ############################################################

  ############################################################
  # Latest intermediate results dirctory
  ############################################################

  def latest_results_directory(self):
    log.debug("latest_results_directory")
    path = os.path.join(self.result_directory,"latest")
    try:
      os.makedirs(path)
    except OSError:
      pass
    return path

  ############################################################
  # report_results
  ############################################################
  def report_results(self, framework, test, results):
    log.debug("report_results: %s - %s" % (framework.name, test))
    log.debug("report_results: %s" % results)

    if test not in self.results['rawData'].keys():
      self.results['rawData'][test] = dict()

    # If results has a size from the parse, then it succeeded.
    if results:
      self.results['rawData'][test][framework.name] = results
      # This may already be set for single-tests
      if framework.name not in self.results['succeeded'][test]:
        self.results['succeeded'][test].append(framework.name)
      # Add this type
      if (os.path.exists(self.get_warning_file(framework.name, test)) and
          framework.name not in self.results['warning'][test]):
        self.results['warning'][test].append(framework.name)
    else:
      # This may already be set for single-tests
      if framework.name not in self.results['failed'][test]:
        self.results['failed'][test].append(framework.name)

  ############################################################
  # End report_results
  ############################################################

  ##########################################################################################
  # Private methods
  ##########################################################################################

  ############################################################
  # Gathers all the tests
  ############################################################
  @property
  def __gather_tests(self):
    log.info("__gather_tests")
    tests = []

    # Assume we are running from FrameworkBenchmarks
    config_files = glob.glob('*/benchmark_config')

    for config_file_name in config_files:
      # Look for the benchmark_config file, this will set up our tests.
      # Its format looks like this:
      #
      # {
      #   "framework": "nodejs",
      #   "tests": [{
      #     "default": {
      #       "setup_file": "setup",
      #       "json_url": "/json"
      #     },
      #     "mysql": {
      #       "setup_file": "setup",
      #       "db_url": "/mysql",
      #       "query_url": "/mysql?queries="
      #     },
      #     ...
      #   }]
      # }
      config = None

      with open(config_file_name, 'r') as config_file:
        # Load json file into config object
        try:
          config = json.load(config_file)
        except:
          log.error("Error loading '%s'" % config_file_name)
          raise

      if config is None:
        continue

      test = framework_test.parse_config(config, os.path.dirname(config_file_name), self)
      # If the user specified which tests to run, then 
      # we can skip over tests that are not in that list
      if self.test == None:
        tests = tests + test
      else:
        for atest in test:
          if atest.name in self.test:
            tests.append(atest)

    tests.sort(key=lambda x: x.name)
    return tests
  ############################################################
  # End __gather_tests
  ############################################################

  ############################################################
  # Gathers all the frameworks
  ############################################################
  def __gather_frameworks(self):
    log.info("__gather_frameworks")

    frameworks = []
    # Loop through each directory (we assume we're being run from the benchmarking root)
    for dirname, dirnames, filenames in os.walk('.'):
      # Look for the benchmark_config file, this will contain our framework name
      # It's format looks like this:
      #
      # {
      #   "framework": "nodejs",
      #   "tests": [{
      #     "default": {
      #       "setup_file": "setup",
      #       "json_url": "/json"
      #     },
      #     "mysql": {
      #       "setup_file": "setup",
      #       "db_url": "/mysql",
      #       "query_url": "/mysql?queries="
      #     },
      #     ...
      #   }]
      # }
      if 'benchmark_config' in filenames:
        config = None
        with open(os.path.join(dirname, 'benchmark_config'), 'r') as config_file:
          # Load json file into config object
          config = json.load(config_file)
        if config == None:
          continue
        frameworks.append(str(config['framework']))
    return frameworks
  ############################################################
  # End __gather_frameworks
  ############################################################

  ############################################################
  # Makes any necessary changes to the server that should be 
  # made before running the tests. This involves setting kernal
  # settings to allow for more connections, or more file
  # descriptiors
  #
  # http://redmine.lighttpd.net/projects/weighttp/wiki#Troubleshooting
  ############################################################
  def __setup_server(self):
    log.info("__setup_server")
    try:
      if os.name == 'nt':
        return True
      (out, err) = WrapLogger(log, logging.DEBUG), WrapLogger(log, logging.ERROR)
      def check(command):
        subprocess.check_call(command, shell=True, stdout=out, stderr=err)

      check(["sudo","bash","-c","cd /sys/devices/system/cpu; ls -d cpu[0-9]*|while read x; do echo performance > $x/cpufreq/scaling_governor; done"])
      check("sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535".rsplit(" "))
      check("sudo sysctl -w net.core.somaxconn=65535".rsplit(" "))
      check("sudo -s ulimit -n 65535".rsplit(" "))
      check("sudo sysctl net.ipv4.tcp_tw_reuse=1".rsplit(" "))
      check("sudo sysctl net.ipv4.tcp_tw_recycle=1".rsplit(" "))
      check("sudo sysctl -w kernel.shmmax=134217728".rsplit(" "))
      check("sudo sysctl -w kernel.shmall=2097152".rsplit(" "))
    except subprocess.CalledProcessError:
      return False
  ############################################################
  # End __setup_server
  ############################################################

  ############################################################
  # Makes any necessary changes to the database machine that 
  # should be made before running the tests. Is very similar
  # to the server setup, but may also include database specific
  # changes.
  ############################################################
  def __setup_database(self):
    log.info("__setup_database")
    (out, err) = WrapLogger(log, logging.DEBUG), WrapLogger(log, logging.ERROR)
    p = subprocess.Popen(self.database_ssh_string, stdin=subprocess.PIPE, shell=True, stdout=out, stderr=err)
    p.communicate("""
      sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
      sudo sysctl -w net.core.somaxconn=65535
      sudo -s ulimit -n 65535
      sudo sysctl net.ipv4.tcp_tw_reuse=1
      sudo sysctl net.ipv4.tcp_tw_recycle=1
      sudo sysctl -w kernel.shmmax=2147483648
      sudo sysctl -w kernel.shmall=2097152
    """)
  ############################################################
  # End __setup_database
  ############################################################

  ############################################################
  # Makes any necessary changes to the client machine that 
  # should be made before running the tests. Is very similar
  # to the server setup, but may also include client specific
  # changes.
  ############################################################
  def __setup_client(self):
    log.info("__setup_client")
    (out, err) = WrapLogger(log, logging.DEBUG), WrapLogger(log, logging.ERROR)
    p = subprocess.Popen(self.client_ssh_string, stdin=subprocess.PIPE, shell=True, stdout=out, stderr=err)
    p.communicate("""
      sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
      sudo sysctl -w net.core.somaxconn=65535
      sudo -s ulimit -n 65535
      sudo sysctl net.ipv4.tcp_tw_reuse=1
      sudo sysctl net.ipv4.tcp_tw_recycle=1
      sudo sysctl -w kernel.shmmax=2147483648
      sudo sysctl -w kernel.shmall=2097152
    """)
  ############################################################
  # End __setup_client
  ############################################################

  ############################################################
  # __run_tests
  #
  # 2013-10-02 ASB  Calls each test passed in tests to
  #                 __run_test in a separate process.  Each
  #                 test is given a set amount of time and if
  #                 kills the child process (and subsequently
  #                 all of its child processes).  Uses
  #                 multiprocessing module.
  ############################################################

  def __run_tests(self, tests):
    log.info("__run_tests")
    log.debug("__run_tests with __name__ = %s",__name__)

    if self.os.lower() == 'windows':
      log.info("Executing __run_tests on Windows")
      for test in tests:
        self.__run_test(test)
    else:
      log.info("Executing __run_tests on Linux")
      # These features do not work on Windows
      for test in tests:
        if __name__ == 'benchmark.benchmarker':
          print Header("Running Test: %s" % test.name)
          test_process = Process(target=self.__run_test, name="Test Runner (%s)" % test.name, args=(test,))
          test_process.start()
          test_process.join(self.run_test_timeout_seconds)
          self.__load_results()  # Load intermediate result from child process
          if(test_process.is_alive()):
            log.warn("Child process for {name} is still alive. Terminating.".format(name=test.name))
            self.__write_intermediate_results(test.name,"__run_test timeout (="+ str(self.run_test_timeout_seconds) + " seconds)")
            test_process.terminate()
          log.handlers = []  # Clean up handlers left by __run_test
    log.info("End __run_tests")

  ############################################################
  # End __run_tests
  ############################################################

  ############################################################
  # __run_test
  #
  # Checks test prerequisites, makes calls into framework_test
  # to orchestrate benchmarking this test, and stores results
  ############################################################
  def __run_test(self, test):
    log.info("__run_test")

    logfile = os.path.join(self.latest_results_directory, 'logs', test.name, 'output.log')
    try:
      os.makedirs(os.path.dirname(logfile))
    except:
      pass

    # Create handler for file logging
    logHandler = logging.FileHandler(logfile, mode='w')
    f = logging.Formatter("%(asctime)s: %(name)-12s: %(levelname)-8s %(message)s")
    logHandler.setFormatter(f)
    logHandler.setLevel(logging.DEBUG)
    log.addHandler(logHandler)

    # Ensure messages of level info will be sent to stdout
    # This is a bit tricky, because we pass our logger to 
    # framework_test, so this means anything logged to it 
    # at INFO or greater is always shown in stdout. This is 
    # good for some things (e.g. validate) and bad for others 
    # (e.g. "in function foo"). Ensure you use logger vs log
    # in framework test properly
    rootStreamHandler = logging.getLogger().handlers[0]
    if rootStreamHandler.level > logging.INFO:
      #class FileFilter(logging.Filter):
      #  def filter(self, record):
      #    if record.filename == "benchmarker.py":
      ##      return True
      #    return False
      streamHandler = logging.StreamHandler()
      streamHandler.setFormatter(logging.Formatter("%(message)s"))
      streamHandler.setLevel(logging.INFO)
      # streamHandler.addFilter(FileFilter())
      log.addHandler(streamHandler)

    if hasattr(test, 'skip') and test.skip.lower() == "true":
      log.info("Skipping %s: benchmark_config specifies to skip this test", test.name)
      return

    if test.os.lower() != self.os.lower() or test.database_os.lower() != self.database_os.lower():
      log.info("Skipping %s: OS or Database OS specified in benchmark_config does not match the current environment", test.name)
      return 
    
    if self.exclude != None and test.name in self.exclude:
      log.info("Excluding %s: Added to the excludes list", test.name)
      return
    
    if self.type != 'all' and not test.contains_type(self.type):
      log.info("Skipping %s: Does not contain test for %s", test.name, self.type)
      return

    log.debug("test.os.lower() = %s  test.database_os.lower() = %s", test.os.lower(), test.database_os.lower())
    log.debug("self.results['frameworks'] != None: %s", self.results['frameworks'] != None)
    log.debug("test.name: %s", test.name)
    log.debug("self.results['completed']: %s", self.results['completed'])

    #if self.results['frameworks'] != None and test.name in self.results['completed']:
    #  log.debug("Skipping %s: Found in latest saved data", test.name)
    #  return

    log.info(Header("Beginning %s" % test.name, top='='))

    # Start this test
    log.info(Header("Starting %s" % test.name))

    try:
      if test.requires_database():
        (out, err) = WrapLogger(log, logging.DEBUG), WrapLogger(log, logging.ERROR)
        p = subprocess.Popen(self.database_ssh_string, stdin=subprocess.PIPE, shell=True, stdout=out, stderr=err)
        log.debug("Restarting database")
        p.communicate("""
          sudo restart mysql
          sudo restart mongodb
		      sudo /etc/init.d/postgresql restart
        """)
        time.sleep(10)

      if self.__is_port_bound(test.port):
        self.__write_intermediate_results(test.name, "port %s is not available before start" % test.port)
        log.error(Header("Error: Port %s is not available, cannot start %s" % (test.port, test.name)))
        return

      result = test.start(log)
      if result != 0: 
        test.stop(log)
        time.sleep(5)
        log.error("ERROR: Problem starting %s", test.name)
        log.error(Header("Stopped %s" % test.name))
        self.__write_intermediate_results(test.name,"<setup.py>#start() returned non-zero")
        return
      
      log.info("Sleeping for %s", self.sleep)
      time.sleep(self.sleep)

      # Verify URLs
      test.verify_urls(log)

      # Benchmark
      if self.mode == "benchmark":
        log.info(Header("Benchmarking %s" % test.name))
        test.benchmark(log)

      # Stop this test
      log.info(Header("Stopping %s" % test.name))
      test.stop(log)
      time.sleep(5)

      if self.__is_port_bound(test.port):
        self.__write_intermediate_results(test.name, "port %s was not released by stop" % test.port)
        log.error(Header("Error: Port %s was not released by stop %s" % (test.port, test.name)))
        return

      log.info(Header("Stopped %s" % test.name))
      time.sleep(5)

      ##########################################################
      # Save results thus far into toolset/benchmark/latest.json
      ##########################################################

      log.info(Header("Saving results through %s" % test.name))
      self.__write_intermediate_results(test.name,time.strftime("%Y%m%d%H%M%S", time.localtime()))
    except (OSError, IOError, subprocess.CalledProcessError) as e:
      self.__write_intermediate_results(test.name,"<setup.py> raised an exception")
      log.error(Header("Subprocess Error %s" % test.name))
      log.error("%s", e)
      log.error("%s", sys.exc_info()[:2])
      log.debug("Subprocess Error Details", exc_info=True)
      try:
        test.stop(log)
      except (subprocess.CalledProcessError) as e:
        self.__write_intermediate_results(test.name,"<setup.py>#stop() raised an error")
        log.error(Header("Subprocess Error: Test .stop() raised exception %s" % test.name))
        log.error("%s", e)
        log.error("%s", sys.exc_info()[:2])
        log.debug("Subprocess Error Details", exc_info=True)
    except (KeyboardInterrupt, SystemExit) as e:
      test.stop(log)
      log.info(Header("Cleaning up..."))
      self.__finish()
      sys.exit()
  ############################################################
  # End __run_test
  ############################################################

  ############################################################
  # __is_port_bound
  # Check if the requested port is available. If it
  # isn't available, then a previous test probably didn't
  # shutdown properly.
  ############################################################
  def __is_port_bound(self, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
      # Try to bind to all IP addresses, this port
      s.bind(("", port))
      # If we get here, we were able to bind successfully,
      # which means the port is free.
    except:
      # If we get an exception, it might be because the port is still bound
      # which would be bad, or maybe it is a privileged port (<1024) and we
      # are not running as root, or maybe the server is gone, but sockets are
      # still in TIME_WAIT (SO_REUSEADDR). To determine which scenario, try to
      # connect.
      try:
        s.connect(("127.0.0.1", port))
        # If we get here, we were able to connect to something, which means
        # that the port is still bound.
        return True
      except:
        # An exception means that we couldn't connect, so a server probably
        # isn't still running on the port.
        pass
    finally:
      s.close()

    return False

  ############################################################
  # End __is_port_bound
  ############################################################

  ############################################################
  # __parse_results
  # Ensures that the system has all necessary software to run
  # the tests. This does not include that software for the individual
  # test, but covers software such as curl and weighttp that
  # are needed.
  ############################################################
  def __parse_results(self, tests):
    log.info("__parse_results")
    
    # Run the method to get the commmit count of each framework.
    self.__count_commits()
   # Call the method which counts the sloc for each framework
    self.__count_sloc()

    # Time to create parsed files
    # Aggregate JSON file
    results_file=os.path.join(self.full_results_directory(), "results.json")
    log.debug("Writing results to %s"%results_file)
    log.debug("Results: %s" % json.dumps(self.results))
    with open(results_file, "w") as f:
      f.write(json.dumps(self.results))

  ############################################################
  # End __parse_results
  ############################################################


  #############################################################
  # __count_sloc
  # This is assumed to be run from the benchmark root directory
  #############################################################
  def __count_sloc(self):
    log.info("__count_sloc")
    all_frameworks = self.__gather_frameworks()
    jsonResult = {}

    for framework in all_frameworks:
      try:
        command = "cloc --list-file=" + framework['directory'] + "/source_code --yaml"
        lineCount = subprocess.check_output(command, shell=True)
        # Find the last instance of the word 'code' in the yaml output. This should
        # be the line count for the sum of all listed files or just the line count
        # for the last file in the case where there's only one file listed.
        lineCount = lineCount[lineCount.rfind('code'):len(lineCount)]
        lineCount = lineCount.strip('code: ')
        lineCount = lineCount[0:lineCount.rfind('comment')]
        jsonResult[framework['name']] = int(lineCount)
      except:
        continue
    self.results['rawData']['slocCounts'] = jsonResult
  ############################################################
  # End __count_sloc
  ############################################################

  ############################################################
  # __count_commits
  ############################################################
  def __count_commits(self):
    log.info("__count_commits")
    all_frameworks = self.__gather_frameworks()

    jsonResult = {}

    for framework in all_frameworks:
      try:
        command = "git rev-list HEAD -- " + framework + " | sort -u | wc -l"
        commitCount = subprocess.check_output(command, shell=True)
        jsonResult[framework] = int(commitCount)
      except:
        continue

    self.results['rawData']['commitCounts'] = jsonResult
    self.commits = jsonResult
  ############################################################
  # End __count_commits
  ############################################################

  ############################################################
  # __write_intermediate_results
  ############################################################
  def __write_intermediate_results(self,test_name,status_message):
    log.debug("__write_intermediate_results: %s reports %s" % (test_name, status_message))
    try:
      self.results["completed"][test_name] = status_message
      latest_results = os.path.join(self.latest_results_directory, 'results.json')
      log.debug("Dumping to %s: %s" % (latest_results, json.dumps(self.results)))
      with open(latest_results, 'w') as f:
        f.write(json.dumps(self.results))
    except (IOError):
      log.error("Error writing results.json")

  ############################################################
  # End __write_intermediate_results
  ############################################################

  def __load_results(self):
    try:
      with open(os.path.join(self.latest_results_directory, 'results.json')) as f:
        self.results = json.load(f)
    except (ValueError, IOError):
      pass

  ############################################################
  # __finish
  ############################################################
  def __finish(self):
    log.info("__finish")
    print "Time to complete: " + str(int(time.time() - self.start_time)) + " seconds"
    print "Results are saved in " + os.path.join(self.result_directory, self.timestamp)

  ############################################################
  # End __finish
  ############################################################

  ##########################################################################################
  # Constructor
  ########################################################################################## 

  ############################################################
  # Initialize the benchmarker. The args are the arguments 
  # parsed via argparser.
  ############################################################
  def __init__(self, args):
    
    self.__dict__.update(args)
    self.start_time = time.time()
    self.run_test_timeout_seconds = 3600
    
    # setup some additional variables
    if self.database_user == None: self.database_user = self.client_user
    if self.database_host == None: self.database_host = self.client_host
    if self.database_identity_file == None: self.database_identity_file = self.client_identity_file

    # Remember root directory
    self.fwroot = setup_util.get_fwroot()

    # setup results and latest_results directories 
    self.result_directory = os.path.join("results", self.name)
    self.latest_results_directory = self.latest_results_directory()
  
    if self.parse != None:
      self.timestamp = self.parse
    else:
      self.timestamp = time.strftime("%Y%m%d%H%M%S", time.localtime())

    # Setup the concurrency levels array. This array goes from
    # starting_concurrency to max concurrency, doubling each time
    self.concurrency_levels = []
    concurrency = self.starting_concurrency
    while concurrency <= self.max_concurrency:
      self.concurrency_levels.append(concurrency)
      concurrency = concurrency * 2

    # Setup query interval array
    # starts at 1, and goes up to max_queries, using the query_interval
    self.query_intervals = []
    queries = 1
    while queries <= self.max_queries:
      self.query_intervals.append(queries)
      if queries == 1:
        queries = 0

      queries = queries + self.query_interval
    
    # Load the latest data
    #self.latest = None
    #try:
    #  with open('toolset/benchmark/latest.json', 'r') as f:
    #    # Load json file into config object
    #    self.latest = json.load(f)
    #    logging.info("toolset/benchmark/latest.json loaded to self.latest")
    #    logging.debug("contents of latest.json: " + str(json.dumps(self.latest)))
    #except IOError:
    #  logging.warn("IOError on attempting to read toolset/benchmark/latest.json")
    #
    #self.results = None
    #try: 
    #  if self.latest != None and self.name in self.latest.keys():
    #    with open(os.path.join(self.result_directory, str(self.latest[self.name]), 'results.json'), 'r') as f:
    #      # Load json file into config object
    #      self.results = json.load(f)
    #except IOError:
    #  pass

    self.results = None
    try:
      with open(os.path.join(self.latest_results_directory, 'results.json'), 'r') as f:
        #Load json file into results object
        self.results = json.load(f)
    except IOError:
      log.warn("results.json for test %s not found.",self.name) 
    
    if self.results == None:
      self.results = dict()
      self.results['name'] = self.name
      self.results['concurrencyLevels'] = self.concurrency_levels
      self.results['queryIntervals'] = self.query_intervals
      self.results['frameworks'] = [t.name for t in self.__gather_tests]
      self.results['duration'] = self.duration
      self.results['rawData'] = dict()
      self.results['rawData']['json'] = dict()
      self.results['rawData']['db'] = dict()
      self.results['rawData']['query'] = dict()
      self.results['rawData']['fortune'] = dict()
      self.results['rawData']['update'] = dict()
      self.results['rawData']['plaintext'] = dict()
      self.results['completed'] = dict()
      self.results['succeeded'] = dict()
      self.results['succeeded']['json'] = []
      self.results['succeeded']['db'] = []
      self.results['succeeded']['query'] = []
      self.results['succeeded']['fortune'] = []
      self.results['succeeded']['update'] = []
      self.results['succeeded']['plaintext'] = []
      self.results['failed'] = dict()
      self.results['failed']['json'] = []
      self.results['failed']['db'] = []
      self.results['failed']['query'] = []
      self.results['failed']['fortune'] = []
      self.results['failed']['update'] = []
      self.results['failed']['plaintext'] = []
      self.results['warning'] = dict()
      self.results['warning']['json'] = []
      self.results['warning']['db'] = []
      self.results['warning']['query'] = []
      self.results['warning']['fortune'] = []
      self.results['warning']['update'] = []
      self.results['warning']['plaintext'] = []
    else:
      #for x in self.__gather_tests():
      #  if x.name not in self.results['frameworks']:
      #    self.results['frameworks'] = self.results['frameworks'] + [x.name]
      # Always overwrite framework list
      self.results['frameworks'] = [t.name for t in self.__gather_tests]

    # Setup the ssh command string
    self.database_ssh_string = "ssh -T -o StrictHostKeyChecking=no " + self.database_user + "@" + self.database_host
    self.client_ssh_string = "ssh -T -o StrictHostKeyChecking=no " + self.client_user + "@" + self.client_host
    if self.database_identity_file != None:
      self.database_ssh_string = self.database_ssh_string + " -i " + self.database_identity_file
    if self.client_identity_file != None:
      self.client_ssh_string = self.client_ssh_string + " -i " + self.client_identity_file

    if self.install is not None:
      install = Installer(self, self.install_strategy)
      install.install_software()

  ############################################################
  # End __init__
  ############################################################
