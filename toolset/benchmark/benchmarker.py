from setup.linux.installer import Installer
from benchmark import framework_test

import os
import json
import subprocess
import time
import textwrap
import pprint
import csv
import sys
import logging
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
    ##########################
    # Get a list of all known
    # tests that we can run.
    ##########################    
    all_tests = self.__gather_tests

    ##########################
    # Setup client/server
    ##########################
    print textwrap.dedent("""
      =====================================================
        Preparing Server, Database, and Client ...
      =====================================================
      """)
    self.__setup_server()
    self.__setup_database()
    self.__setup_client()

    # Check if wrk (and wrk-pipeline) is installed and executable, if not, raise an exception
    checkWrk = "hash wrk 2>/dev/null"
    checkWrkPipeline = "hash wrk-pipeline 2>/dev/null"
    host = self.client_user + "@" + self.client_host
    if subprocess.call(['ssh', host, checkWrk]) or subprocess.call(['ssh', host, checkWrkPipeline]):
      raise Exception("wrk and/or wrk-pipeline are not properly installed. Not running tests.")
    ## Check if wrk (and wrk-pipeline) is installed and executable, if not, raise an exception
    #if not (os.access("/usr/local/bin/wrk", os.X_OK) and os.access("/usr/local/bin/wrk-pipeline", os.X_OK)):
    #  raise Exception("wrk and/or wrk-pipeline are not properly installed. Not running tests.")


    ##########################
    # Run tests
    ##########################
    print textwrap.dedent("""
      =====================================================
        Running Tests ...
      =====================================================
      """)
    self.__run_tests(all_tests)

    ##########################
    # Parse results
    ##########################  
    if self.mode == "benchmark":
      print textwrap.dedent("""
      =====================================================
        Parsing Results ...
      =====================================================
      """)
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
          print("Error loading '%s'." % config_file_name)
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
    try:
      if os.name == 'nt':
        return True
      subprocess.check_call(["sudo","bash","-c","cd /sys/devices/system/cpu; ls -d cpu*|while read x; do echo performance > $x/cpufreq/scaling_governor; done"])
      subprocess.check_call("sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535".rsplit(" "))
      subprocess.check_call("sudo sysctl -w net.core.somaxconn=65535".rsplit(" "))
      subprocess.check_call("sudo -s ulimit -n 65535".rsplit(" "))
      subprocess.check_call("sudo sysctl net.ipv4.tcp_tw_reuse=1".rsplit(" "))
      subprocess.check_call("sudo sysctl net.ipv4.tcp_tw_recycle=1".rsplit(" "))
      subprocess.check_call("sudo sysctl -w kernel.shmmax=134217728".rsplit(" "))
      subprocess.check_call("sudo sysctl -w kernel.shmall=2097152".rsplit(" "))
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
    p = subprocess.Popen(self.database_ssh_string, stdin=subprocess.PIPE, shell=True)
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
    p = subprocess.Popen(self.client_ssh_string, stdin=subprocess.PIPE, shell=True)
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
    logging.debug("Start __run_tests.")
    logging.debug("__name__ = %s",__name__)

    if self.os.lower() == 'windows':
      logging.debug("Executing __run_tests on Windows")
      for test in tests:
        self.__run_test(test)
    else:
      logging.debug("Executing __run_tests on Linux")
      # These features do not work on Windows
      for test in tests:
        if __name__ == 'benchmark.benchmarker':
          print textwrap.dedent("""
            -----------------------------------------------------
              Running Test: {name} ...
            -----------------------------------------------------
            """.format(name=test.name))
          test_process = Process(target=self.__run_test, args=(test,))
          test_process.start()
          test_process.join(self.run_test_timeout_seconds)
          if(test_process.is_alive()):
            logging.debug("Child process for {name} is still alive. Terminating.".format(name=test.name))
            self.__write_intermediate_results(test.name,"__run_test timeout (="+ str(self.run_test_timeout_seconds) + " seconds)")
            test_process.terminate()
    logging.debug("End __run_tests.")

  ############################################################
  # End __run_tests
  ############################################################

  ############################################################
  # __run_test
  # 2013-10-02 ASB  Previously __run_tests.  This code now only
  #                 processes a single test.
  #
  # Ensures that the system has all necessary software to run
  # the tests. This does not include that software for the individual
  # test, but covers software such as curl and weighttp that
  # are needed.
  ############################################################
  def __run_test(self, test):
    try:
      os.makedirs(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name)))
    except:
      pass
    with open(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name), 'out.txt'), 'w') as out, \
         open(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name), 'err.txt'), 'w') as err:
      if hasattr(test, 'skip'):
        if test.skip.lower() == "true":
          out.write("Test {name} benchmark_config specifies to skip this test. Skipping.\n".format(name=test.name))
          return

      if test.os.lower() != self.os.lower() or test.database_os.lower() != self.database_os.lower():
        # the operating system requirements of this test for the
        # application server or the database server don't match
        # our current environment
        out.write("OS or Database OS specified in benchmark_config does not match the current environment. Skipping.\n")
        return 
      
      # If the test is in the excludes list, we skip it
      if self.exclude != None and test.name in self.exclude:
        out.write("Test {name} has been added to the excludes list. Skipping.\n".format(name=test.name))
        return
      
      # If the test does not contain an implementation of the current test-type, skip it
      if self.type != 'all' and not test.contains_type(self.type):
        out.write("Test type {type} does not contain an implementation of the current test-type. Skipping.\n".format(type=self.type))
        return

      out.write("test.os.lower() = {os}  test.database_os.lower() = {dbos}\n".format(os=test.os.lower(),dbos=test.database_os.lower()))
      out.write("self.results['frameworks'] != None: {val}\n".format(val=str(self.results['frameworks'] != None)))
      out.write("test.name: {name}\n".format(name=str(test.name)))
      out.write("self.results['completed']: {completed}\n".format(completed=str(self.results['completed'])))
      if self.results['frameworks'] != None and test.name in self.results['completed']:
        out.write('Framework {name} found in latest saved data. Skipping.\n'.format(name=str(test.name)))
        return

      out.flush()

      out.write( textwrap.dedent("""
      =====================================================
        Beginning {name}
      -----------------------------------------------------
      """.format(name=test.name)) )
      out.flush()

      ##########################
      # Start this test
      ##########################  
      out.write( textwrap.dedent("""
      -----------------------------------------------------
        Starting {name}
      -----------------------------------------------------
      """.format(name=test.name)) )
      out.flush()
      try:
        if test.requires_database():
          p = subprocess.Popen(self.database_ssh_string, stdin=subprocess.PIPE, stdout=out, stderr=err, shell=True)
          p.communicate("""
            sudo restart mysql
            sudo restart mongodb
  		      sudo /etc/init.d/postgresql restart
          """)
          time.sleep(10)

        if self.__is_port_bound(test.port):
          self.__write_intermediate_results(test.name, "port " + str(test.port) + " is not available before start")
          err.write( textwrap.dedent("""
            ---------------------------------------------------------
              Error: Port {port} is not available before start {name}
            ---------------------------------------------------------
            """.format(name=test.name, port=str(test.port))) )
          err.flush()
          return

        result = test.start(out, err)
        if result != 0: 
          test.stop(out, err)
          time.sleep(5)
          err.write( "ERROR: Problem starting {name}\n".format(name=test.name) )
          err.write( textwrap.dedent("""
            -----------------------------------------------------
              Stopped {name}
            -----------------------------------------------------
            """.format(name=test.name)) )
          err.flush()
          self.__write_intermediate_results(test.name,"<setup.py>#start() returned non-zero")
          return
        
        time.sleep(self.sleep)

        ##########################
        # Verify URLs
        ##########################
        test.verify_urls(out, err)
        out.flush()
        err.flush()

        ##########################
        # Benchmark this test
        ##########################
        if self.mode == "benchmark":
          out.write( textwrap.dedent("""
            -----------------------------------------------------
              Benchmarking {name} ...
            -----------------------------------------------------
            """.format(name=test.name)) )
          out.flush()
          test.benchmark(out, err)
          out.flush()
          err.flush()

        ##########################
        # Stop this test
        ##########################
        out.write( textwrap.dedent("""
        -----------------------------------------------------
          Stopping {name}
        -----------------------------------------------------
        """.format(name=test.name)) )
        out.flush()
        test.stop(out, err)
        out.flush()
        err.flush()
        time.sleep(5)

        if self.__is_port_bound(test.port):
          self.__write_intermediate_results(test.name, "port " + str(test.port) + " was not released by stop")
          err.write( textwrap.dedent("""
            -----------------------------------------------------
              Error: Port {port} was not released by stop {name}
            -----------------------------------------------------
            """.format(name=test.name, port=str(test.port))) )
          err.flush()
          return

        out.write( textwrap.dedent("""
        -----------------------------------------------------
          Stopped {name}
        -----------------------------------------------------
        """.format(name=test.name)) )
        out.flush()
        time.sleep(5)

        ##########################################################
        # Save results thus far into toolset/benchmark/latest.json
        ##########################################################

        out.write( textwrap.dedent("""
        ----------------------------------------------------
        Saving results through {name}
        ----------------------------------------------------
        """.format(name=test.name)) )
        out.flush()
        self.__write_intermediate_results(test.name,time.strftime("%Y%m%d%H%M%S", time.localtime()))
      except (OSError, IOError, subprocess.CalledProcessError) as e:
        self.__write_intermediate_results(test.name,"<setup.py> raised an exception")
        err.write( textwrap.dedent("""
        -----------------------------------------------------
          Subprocess Error {name}
        -----------------------------------------------------
        {err}
        {trace}
        """.format(name=test.name, err=e, trace=sys.exc_info()[:2])) )
        err.flush()
        try:
          test.stop(out, err)
        except (subprocess.CalledProcessError) as e:
          self.__write_intermediate_results(test.name,"<setup.py>#stop() raised an error")
          err.write( textwrap.dedent("""
          -----------------------------------------------------
            Subprocess Error: Test .stop() raised exception {name}
          -----------------------------------------------------
          {err}
          {trace}
          """.format(name=test.name, err=e, trace=sys.exc_info()[:2])) )
          err.flush()
      except (KeyboardInterrupt, SystemExit) as e:
        test.stop(out)
        out.write( """
        -----------------------------------------------------
          Cleaning up....
        -----------------------------------------------------
        """)
        out.flush()
        self.__finish()
        sys.exit()

      out.close()
      err.close()

  ############################################################
  # End __run_tests
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
    # Run the method to get the commmit count of each framework.
    self.__count_commits()
   # Call the method which counts the sloc for each framework
    self.__count_sloc()

    # Time to create parsed files
    # Aggregate JSON file
    with open(os.path.join(self.full_results_directory(), "results.json"), "w") as f:
      f.write(json.dumps(self.results))

  ############################################################
  # End __parse_results
  ############################################################


  #############################################################
  # __count_sloc
  # This is assumed to be run from the benchmark root directory
  #############################################################
  def __count_sloc(self):
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
    try:
      self.results["completed"][test_name] = status_message
      with open(os.path.join(self.latest_results_directory, 'results.json'), 'w') as f:
        f.write(json.dumps(self.results))
    except (IOError):
      logging.error("Error writing results.json")

  ############################################################
  # End __write_intermediate_results
  ############################################################

  ############################################################
  # __finish
  ############################################################
  def __finish(self):
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

    # setup logging
    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)
    
    # setup some additional variables
    if self.database_user == None: self.database_user = self.client_user
    if self.database_host == None: self.database_host = self.client_host
    if self.database_identity_file == None: self.database_identity_file = self.client_identity_file

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
      logging.warn("results.json for test %s not found.",self.name) 
    
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

    if self.install_software:
      install = Installer(self)
      install.install_software()

  ############################################################
  # End __init__
  ############################################################
