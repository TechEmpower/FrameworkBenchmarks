from setup.linux.installer import Installer
from setup.linux import setup_util

from benchmark import framework_test
from utils import header
from utils import gather_tests

import os
import json
import subprocess
import traceback
import time
import pprint
import csv
import sys
import logging
import socket
from multiprocessing import Process
from datetime import datetime

# Cross-platform colored text
from colorama import Fore, Back, Style

# Text-based progress indicators
import progressbar

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
    print header("Preparing Server, Database, and Client ...", top='=', bottom='=')
    self.__setup_server()
    self.__setup_database()
    self.__setup_client()

    ## Check if wrk (and wrk-pipeline) is installed and executable, if not, raise an exception
    #if not (os.access("/usr/local/bin/wrk", os.X_OK) and os.access("/usr/local/bin/wrk-pipeline", os.X_OK)):
    #  raise Exception("wrk and/or wrk-pipeline are not properly installed. Not running tests.")

    ##########################
    # Run tests
    ##########################
    print header("Running Tests...", top='=', bottom='=')
    result = self.__run_tests(all_tests)

    ##########################
    # Parse results
    ##########################  
    if self.mode == "benchmark":
      print header("Parsing Results ...", top='=', bottom='=')
      self.__parse_results(all_tests)

    self.__finish()
    return result

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
  # get_stats_file(test_name, test_type)
  # returns the stats file name for this test_name and 
  # test_type timestamp/test_type/test_name/raw 
  ############################################################
  def get_stats_file(self, test_name, test_type):
    return os.path.join(self.result_directory, self.timestamp, test_type, test_name, "stats")
  ############################################################
  # End get_stats_file
  ############################################################


  ############################################################
  # stats_file(test_name, test_type)
  # returns the stats file for this test_name and test_type
  # timestamp/test_type/test_name/raw 
  ############################################################
  def stats_file(self, test_name, test_type):
      path = self.get_stats_file(test_name, test_type)
      try:
        os.makedirs(os.path.dirname(path))
      except OSError:
        pass
      return path
  ############################################################
  # End stats_file
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
  # report_verify_results
  # Used by FrameworkTest to add verification details to our results
  #
  # TODO: Technically this is an IPC violation - we are accessing
  # the parent process' memory from the child process
  ############################################################
  def report_verify_results(self, framework, test, result):
    if framework.name not in self.results['verify'].keys():
      self.results['verify'][framework.name] = dict()
    self.results['verify'][framework.name][test] = result

  ############################################################
  # report_benchmark_results
  # Used by FrameworkTest to add benchmark data to this
  #
  # TODO: Technically this is an IPC violation - we are accessing
  # the parent process' memory from the child process
  ############################################################
  def report_benchmark_results(self, framework, test, results):
    if test not in self.results['rawData'].keys():
      self.results['rawData'][test] = dict()

    # If results has a size from the parse, then it succeeded.
    if results:
      self.results['rawData'][test][framework.name] = results

      # This may already be set for single-tests
      if framework.name not in self.results['succeeded'][test]:
        self.results['succeeded'][test].append(framework.name)
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
    tests = gather_tests(include=self.test, 
      exclude=self.exclude,
      benchmarker=self)

    # If the tests have been interrupted somehow, then we want to resume them where we left
    # off, rather than starting from the beginning
    if os.path.isfile('current_benchmark.txt'):
        with open('current_benchmark.txt', 'r') as interrupted_benchmark:
            interrupt_bench = interrupted_benchmark.read()
            for index, atest in enumerate(tests):
                if atest.name == interrupt_bench:
                    tests = tests[index:]
                    break
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
      subprocess.check_call(["sudo","bash","-c","cd /sys/devices/system/cpu; ls -d cpu[0-9]*|while read x; do echo performance > $x/cpufreq/scaling_governor; done"])
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
    if len(tests) == 0:
      return 0

    logging.debug("Start __run_tests.")
    logging.debug("__name__ = %s",__name__)

    error_happened = False
    if self.os.lower() == 'windows':
      logging.debug("Executing __run_tests on Windows")
      for test in tests:
        with open('current_benchmark.txt', 'w') as benchmark_resume_file:
          benchmark_resume_file.write(test.name)
        if self.__run_test(test) != 0:
          error_happened = True
    else:
      logging.debug("Executing __run_tests on Linux")

      # Setup a nice progressbar and ETA indicator
      widgets = [self.mode, ': ',  progressbar.Percentage(), 
                 ' ', progressbar.Bar(),
                 ' Rough ', progressbar.ETA()]
      pbar = progressbar.ProgressBar(widgets=widgets, maxval=len(tests)).start()
      pbar_test = 0

      # These features do not work on Windows
      for test in tests:
        pbar.update(pbar_test)
        pbar_test = pbar_test + 1
        if __name__ == 'benchmark.benchmarker':
          print header("Running Test: %s" % test.name)
          with open('current_benchmark.txt', 'w') as benchmark_resume_file:
            benchmark_resume_file.write(test.name)
          test_process = Process(target=self.__run_test, name="Test Runner (%s)" % test.name, args=(test,))
          test_process.start()
          test_process.join(self.run_test_timeout_seconds)
          self.__load_results()  # Load intermediate result from child process
          if(test_process.is_alive()):
            logging.debug("Child process for {name} is still alive. Terminating.".format(name=test.name))
            self.__write_intermediate_results(test.name,"__run_test timeout (="+ str(self.run_test_timeout_seconds) + " seconds)")
            test_process.terminate()
            test_process.join()
          if test_process.exitcode != 0:
            error_happened = True
      pbar.finish()
    if os.path.isfile('current_benchmark.txt'):
      os.remove('current_benchmark.txt')
    logging.debug("End __run_tests.")

    if error_happened:
      return 1
    return 0
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
    
    # Used to capture return values 
    def exit_with_code(code):
      if self.os.lower() == 'windows':
        return code
      else:
        sys.exit(code)

    try:
      os.makedirs(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name)))
    except:
      pass
    with open(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name), 'out.txt'), 'w') as out, \
         open(os.path.join(self.latest_results_directory, 'logs', "{name}".format(name=test.name), 'err.txt'), 'w') as err:
      if hasattr(test, 'skip'):
        if test.skip.lower() == "true":
          out.write("Test {name} benchmark_config specifies to skip this test. Skipping.\n".format(name=test.name))
          return exit_with_code(0)

      if test.os.lower() != self.os.lower() or test.database_os.lower() != self.database_os.lower():
        # the operating system requirements of this test for the
        # application server or the database server don't match
        # our current environment
        out.write("OS or Database OS specified in benchmark_config does not match the current environment. Skipping.\n")
        return exit_with_code(0)
      
      # If the test is in the excludes list, we skip it
      if self.exclude != None and test.name in self.exclude:
        out.write("Test {name} has been added to the excludes list. Skipping.\n".format(name=test.name))
        return exit_with_code(0)
      
      # If the test does not contain an implementation of the current test-type, skip it
      if self.type != 'all' and not test.contains_type(self.type):
        out.write("Test type {type} does not contain an implementation of the current test-type. Skipping.\n".format(type=self.type))
        return exit_with_code(0)

      out.write("test.os.lower() = {os}  test.database_os.lower() = {dbos}\n".format(os=test.os.lower(),dbos=test.database_os.lower()))
      out.write("self.results['frameworks'] != None: {val}\n".format(val=str(self.results['frameworks'] != None)))
      out.write("test.name: {name}\n".format(name=str(test.name)))
      out.write("self.results['completed']: {completed}\n".format(completed=str(self.results['completed'])))
      if self.results['frameworks'] != None and test.name in self.results['completed']:
        out.write('Framework {name} found in latest saved data. Skipping.\n'.format(name=str(test.name)))
        return exit_with_code(1)
      out.flush()

      out.write(header("Beginning %s" % test.name, top='='))
      out.flush()

      ##########################
      # Start this test
      ##########################  
      out.write(header("Starting %s" % test.name))
      out.flush()
      try:
        if test.requires_database():
          p = subprocess.Popen(self.database_ssh_string, stdin=subprocess.PIPE, stdout=out, stderr=err, shell=True)
          p.communicate("""
            sudo restart mysql
            sudo restart mongodb
            sudo service redis-server restart
            sudo /etc/init.d/postgresql restart
          """)
          time.sleep(10)

        if self.__is_port_bound(test.port):
          self.__write_intermediate_results(test.name, "port " + str(test.port) + " is not available before start")
          err.write(header("Error: Port %s is not available, cannot start %s" % (test.port, test.name)))
          err.flush()
          return exit_with_code(1)

        result = test.start(out, err)
        if result != 0: 
          test.stop(out, err)
          time.sleep(5)
          err.write( "ERROR: Problem starting {name}\n".format(name=test.name) )
          err.write(header("Stopped %s" % test.name))
          err.flush()
          self.__write_intermediate_results(test.name,"<setup.py>#start() returned non-zero")
          return exit_with_code(1)
        
        time.sleep(self.sleep)

        ##########################
        # Verify URLs
        ##########################
        passed_verify = test.verify_urls(out, err)
        out.flush()
        err.flush()

        ##########################
        # Benchmark this test
        ##########################
        if self.mode == "benchmark":
          out.write(header("Benchmarking %s" % test.name))
          out.flush()
          test.benchmark(out, err)
          out.flush()
          err.flush()

        ##########################
        # Stop this test
        ##########################
        out.write(header("Stopping %s" % test.name))
        out.flush()
        test.stop(out, err)
        out.flush()
        err.flush()
        time.sleep(5)

        if self.__is_port_bound(test.port):
          self.__write_intermediate_results(test.name, "port " + str(test.port) + " was not released by stop")
          err.write(header("Error: Port %s was not released by stop %s" % (test.port, test.name)))
          err.flush()
          return exit_with_code(1)

        out.write(header("Stopped %s" % test.name))
        out.flush()
        time.sleep(5)

        ##########################################################
        # Save results thus far into toolset/benchmark/latest.json
        ##########################################################

        out.write(header("Saving results through %s" % test.name))
        out.flush()
        self.__write_intermediate_results(test.name,time.strftime("%Y%m%d%H%M%S", time.localtime()))

        if self.mode == "verify" and not passed_verify:
          print "Failed verify!"
          return exit_with_code(1)
      except (OSError, IOError, subprocess.CalledProcessError) as e:
        self.__write_intermediate_results(test.name,"<setup.py> raised an exception")
        err.write(header("Subprocess Error %s" % test.name))
        traceback.print_exc(file=err)
        err.flush()
        try:
          test.stop(out, err)
        except (subprocess.CalledProcessError) as e:
          self.__write_intermediate_results(test.name,"<setup.py>#stop() raised an error")
          err.write(header("Subprocess Error: Test .stop() raised exception %s" % test.name))
          traceback.print_exc(file=err)
          err.flush()
        out.close()
        err.close()
        return exit_with_code(1)
      # TODO - subprocess should not catch this exception!
      # Parent process should catch it and cleanup/exit
      except (KeyboardInterrupt) as e:
        test.stop(out, err)
        out.write(header("Cleaning up..."))
        out.flush()
        self.__finish()
        sys.exit(1)

      out.close()
      err.close()
      return exit_with_code(0)

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
    tests = self.__gather_tests
    # Normally you don't have to use Fore.BLUE before each line, but 
    # Travis-CI seems to reset color codes on newline (see travis-ci/travis-ci#2692)
    # or stream flush, so we have to ensure that the color code is printed repeatedly
    prefix = Fore.CYAN
    for line in header("Verification Summary", top='=', bottom='').split('\n'):
      print prefix + line
    for test in tests:
      print prefix + "| Test: %s" % test.name
      if test.name in self.results['verify'].keys():
        for test_type, result in self.results['verify'][test.name].iteritems():
          if result.upper() == "PASS":
            color = Fore.GREEN
          elif result.upper() == "WARN":
            color = Fore.YELLOW
          else:
            color = Fore.RED
          print prefix + "|       " + test_type.ljust(11) + ' : ' + color + result.upper()
      else:
        print prefix + "|      " + Fore.RED + "NO RESULTS (Did framework launch?)"
    print prefix + header('', top='', bottom='=') + Style.RESET_ALL

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
    logging.basicConfig(stream=sys.stderr, level=logging.INFO)
    
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
      self.results['verify'] = dict()
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
