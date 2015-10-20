from benchmark.fortune_html_parser import FortuneHTMLParser
from setup.linux import setup_util
from benchmark.test_types import *

import importlib
import os
import subprocess
import time
import re
from pprint import pprint
import sys
import traceback
import json
import logging
import csv
import shlex
import math
from collections import OrderedDict
from threading import Thread
from threading import Event

from utils import header

# Cross-platform colored text
from colorama import Fore, Back, Style
from datetime import datetime
from datetime import timedelta

class FrameworkTest:
  headers_template = "-H 'Host: localhost' -H '{accept}' -H 'Connection: keep-alive'"
 
  # Used for test types that require no pipelining or query string params.
  concurrency_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}"
    sleep 5

    echo ""
    echo "---------------------------------------------------------"
    echo " Synchronizing time"
    echo "---------------------------------------------------------"
    echo ""
    ntpdate -s pool.ntp.org

    for c in {levels}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Concurrency: $c for {name}"
      echo " {wrk} {headers} --latency -d {duration} -c $c --timeout $c -t $(($c>{max_threads}?{max_threads}:$c)) \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      STARTTIME=$(date +"%s")
      {wrk} {headers} --latency -d {duration} -c $c --timeout $c -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url}
      echo "STARTTIME $STARTTIME"
      echo "ENDTIME $(date +"%s")"
      sleep 2
    done
  """
  # Used for test types that require pipelining.
  pipeline_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}"
    sleep 5

    echo ""
    echo "---------------------------------------------------------"
    echo " Synchronizing time"
    echo "---------------------------------------------------------"
    echo ""
    ntpdate -s pool.ntp.org

    for c in {levels}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Concurrency: $c for {name}"
      echo " {wrk} {headers} --latency -d {duration} -c $c --timeout $c -t $(($c>{max_threads}?{max_threads}:$c)) \"http://{server_host}:{port}{url}\" -s ~/pipeline.lua -- {pipeline}"
      echo "---------------------------------------------------------"
      echo ""
      STARTTIME=$(date +"%s")
      {wrk} {headers} --latency -d {duration} -c $c --timeout $c -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url} -s ~/pipeline.lua -- {pipeline}
      echo "STARTTIME $STARTTIME"
      echo "ENDTIME $(date +"%s")"
      sleep 2
    done
  """
  # Used for test types that require a database - 
  # These tests run at a static concurrency level and vary the size of
  # the query sent with each request
  query_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " wrk {headers} --latency -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} --latency -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}2"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}2"
    sleep 5

    echo ""
    echo "---------------------------------------------------------"
    echo " Synchronizing time"
    echo "---------------------------------------------------------"
    echo ""
    ntpdate -s pool.ntp.org

    for c in {levels}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Queries: $c for {name}"
      echo " wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}$c\""
      echo "---------------------------------------------------------"
      echo ""
      STARTTIME=$(date +"%s")
      wrk {headers} --latency -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}$c"
      echo "STARTTIME $STARTTIME"
      echo "ENDTIME $(date +"%s")"
      sleep 2
    done
  """

  ############################################################
  # start(benchmarker)
  # Start the test using it's setup file
  ############################################################
  def start(self, out, err):

    # Setup environment variables    
    logDir = os.path.join(self.fwroot, self.benchmarker.latest_results_directory, 'logs', self.name.lower())
    bash_functions_path= os.path.join(self.fwroot, 'toolset/setup/linux/bash_functions.sh')
    setup_util.replace_environ(config='$FWROOT/config/benchmark_profile', 
              command='''\
              export TROOT=%s       &&  \
              export IROOT=%s       &&  \
              export DBHOST=%s      &&  \
              export LOGDIR=%s      &&  \
              export MAX_THREADS=%s &&  \
              export MAX_CONCURRENCY=%s \
              ''' % (
                self.directory, 
                self.install_root, 
                self.database_host, 
                logDir,
                self.benchmarker.threads,
                max(self.benchmarker.concurrency_levels)))

    # Always ensure that IROOT belongs to the runner_user
    chown = "sudo chown -R %s:%s %s" % (self.benchmarker.runner_user,
      self.benchmarker.runner_user, os.path.join(self.fwroot, self.install_root))
    subprocess.check_call(chown, shell=True, cwd=self.fwroot, executable='/bin/bash')

    # Run the module start inside parent of TROOT
    #  - we use the parent as a historical accident, a number of tests
    # refer to their TROOT maually still
    previousDir = os.getcwd()
    os.chdir(os.path.dirname(self.troot))
    logging.info("Running setup module start (cwd=%s)", self.directory)
      
    # Run the start script for the test as the "testrunner" user
    # 
    # `sudo` - Switching user requires superuser privs
    #   -u [username] The username
    #   -E Preserves the current environment variables
    #   -H Forces the home var (~) to be reset to the user specified
    # `stdbuf` - Disable buffering, send output to python ASAP
    #   -o0 zero-sized buffer for stdout
    #   -e0 zero-sized buffer for stderr
    # `bash` - Run the setup.sh script using bash
    #   -e Force bash to exit on first error
    #   -x Turn on bash tracing e.g. print commands before running
    #
    # Most servers do not output to stdout/stderr while serving 
    # requests so there is no performance hit from disabling 
    # output buffering. This disabling is necessary to 
    # a) allow TFB to show output in real time and b) avoid loosing 
    # output in the buffer when the testrunner processes are forcibly 
    # killed
    # 
    # See http://www.pixelbeat.org/programming/stdio_buffering/
    # See https://blogs.gnome.org/markmc/2013/06/04/async-io-and-python/
    # See http://eyalarubas.com/python-subproc-nonblock.html
    command = 'sudo -u %s -E -H stdbuf -o0 -e0 bash -exc "source %s && source %s.sh"' % (
      self.benchmarker.runner_user,
      bash_functions_path, 
      os.path.join(self.troot, self.setup_file))
    
    debug_command = '''\
      export FWROOT=%s          &&  \\
      export TROOT=%s           &&  \\
      export IROOT=%s           &&  \\
      export DBHOST=%s          &&  \\
      export LOGDIR=%s          &&  \\
      export MAX_THREADS=%s     &&  \\
      export MAX_CONCURRENCY=%s && \\
      cd %s && \\
      %s''' % (self.fwroot, 
        self.directory, 
        self.install_root, 
        self.database_host,
        logDir,
        self.benchmarker.threads, 
        self.directory,
        max(self.benchmarker.concurrency_levels),
        command)
    logging.info("To run %s manually, copy/paste this:\n%s", self.name, debug_command)


    def tee_output(prefix, line):
      # Needs to be one atomic write
      # Explicitly use UTF-8 as it's the most common framework output 
      # TODO improve encoding handling 
      line = prefix.encode('utf-8') + line

      # Log to current terminal
      sys.stdout.write(line)
      sys.stdout.flush()
      # logging.error("".join([prefix, line]))

      out.write(line)
      out.flush()

    # Start the setup.sh command
    p = subprocess.Popen(command, cwd=self.directory, 
          shell=True, stdout=subprocess.PIPE, 
          stderr=subprocess.STDOUT)
    nbsr = setup_util.NonBlockingStreamReader(p.stdout, 
      "%s: %s.sh and framework processes have terminated" % (self.name, self.setup_file))

    # Set a limit on total execution time of setup.sh
    timeout = datetime.now() + timedelta(minutes = 105)
    time_remaining = timeout - datetime.now()

    # Need to print to stdout once every 10 minutes or Travis-CI will abort
    travis_timeout = datetime.now() + timedelta(minutes = 5)

    # Flush output until setup.sh work is finished. This is 
    # either a) when setup.sh exits b) when the port is bound
    # c) when we run out of time. Note that 'finished' doesn't 
    # guarantee setup.sh process is dead - the OS may choose to make 
    # setup.sh a zombie process if it still has living children
    #
    # Note: child processes forked (using &) will remain alive 
    # after setup.sh has exited. The will have inherited the 
    # stdout/stderr descriptors and will be directing their 
    # output to the pipes. 
    #
    prefix = "Setup %s: " % self.name
    while (p.poll() is None
      and not self.benchmarker.is_port_bound(self.port)
      and not time_remaining.total_seconds() < 0):
      
      # The conditions above are slow to check, so 
      # we will delay output substantially if we only
      # print one line per condition check. 
      # Adding a tight loop here mitigates the effect, 
      # ensuring that most of the output directly from 
      # setup.sh is sent to tee_output before the outer
      # loop exits and prints things like "setup.sh exited"
      # 
      for i in xrange(10):
        try:
          line = nbsr.readline(0.05)
          if line:
            tee_output(prefix, line)

            # Reset Travis-CI timer
            travis_timeout = datetime.now() + timedelta(minutes = 5)
        except setup_util.EndOfStream:
          tee_output(prefix, "Setup has terminated\n")
          break
      time_remaining = timeout - datetime.now()

      if (travis_timeout - datetime.now()).total_seconds() < 0:
        sys.stdout.write(prefix + 'Printing so Travis-CI does not time out\n')
        sys.stdout.write(prefix + "Status: Poll: %s, Port %s bound: %s, Time Left: %s\n" % (
          p.poll(), self.port, self.benchmarker.is_port_bound(self.port), time_remaining))
        sys.stdout.flush()
        travis_timeout = datetime.now() + timedelta(minutes = 5)

    # Did we time out?
    if time_remaining.total_seconds() < 0: 
      tee_output(prefix, "%s.sh timed out!! Aborting...\n" % self.setup_file)
      p.kill()
      return 1

    # What's our return code? 
    # If setup.sh has terminated, use that code
    # Otherwise, detect if the port was bound
    tee_output(prefix, "Status: Poll: %s, Port %s bound: %s, Time Left: %s\n" % (
      p.poll(), self.port, self.benchmarker.is_port_bound(self.port), time_remaining))
    retcode = (p.poll() if p.poll() is not None else 0 if self.benchmarker.is_port_bound(self.port) else 1)
    if p.poll() is not None:
      tee_output(prefix, "%s.sh process exited naturally with %s\n" % (self.setup_file, p.poll()))
    elif self.benchmarker.is_port_bound(self.port):
      tee_output(prefix, "Bound port detected on %s\n" % self.port)

    # Before we return control to the benchmarker, spin up a 
    # thread to keep an eye on the pipes in case the running 
    # framework uses stdout/stderr. Once all processes accessing
    # the subprocess.PIPEs are dead, this thread will terminate. 
    # Use a different prefix to indicate this is the framework 
    # speaking
    prefix = "Server %s: " % self.name
    def watch_child_pipes(nbsr, prefix):
      while True:
        try:
          line = nbsr.readline(60)
          if line:
            tee_output(prefix, line)
        except setup_util.EndOfStream:
          tee_output(prefix, "Framework processes have terminated\n")
          return

    watch_thread = Thread(target = watch_child_pipes,
      args = (nbsr, prefix))
    watch_thread.daemon = True
    watch_thread.start()

    logging.info("Executed %s.sh, returning %s", self.setup_file, retcode)
    os.chdir(previousDir)

    return retcode
  ############################################################
  # End start
  ############################################################

  ############################################################
  # verify_urls
  # Verifys each of the URLs for this test. THis will sinply 
  # curl the URL and check for it's return status. 
  # For each url, a flag will be set on this object for whether
  # or not it passed
  # Returns True if all verifications succeeded
  ############################################################
  def verify_urls(self, out, err):
    result = True
    
    def verify_type(test_type):
      
      test = self.runTests[test_type]
      test.setup_out_err(out, err)
      out.write(header("VERIFYING %s" % test_type.upper()))
      
      base_url = "http://%s:%s" % (self.benchmarker.server_host, self.port)
      
      try:
        results = test.verify(base_url)
      except Exception as e:
        results = [('fail',"""Caused Exception in TFB
          This almost certainly means your return value is incorrect, 
          but also that you have found a bug. Please submit an issue
          including this message: %s\n%s""" % (e, traceback.format_exc()), 
          base_url)]
        logging.warning("Verifying test %s for %s caused an exception: %s", test_type, self.name, e)
        traceback.format_exc()

      test.failed = any(result is 'fail' for (result, reason, url) in results)
      test.warned = any(result is 'warn' for (result, reason, url) in results)
      test.passed = all(result is 'pass' for (result, reason, url) in results)
      
      def output_result(result, reason, url):
        specific_rules_url = "http://frameworkbenchmarks.readthedocs.org/en/latest/Project-Information/Framework-Tests/#specific-test-requirements"
        color = Fore.GREEN
        if result.upper() == "WARN":
          color = Fore.YELLOW
        elif result.upper() == "FAIL":
          color = Fore.RED

        out.write(("   " + color + "%s" + Style.RESET_ALL + " for %s\n") % (result.upper(), url))
        print ("   " + color + "%s" + Style.RESET_ALL + " for %s\n") % (result.upper(), url)
        if reason is not None and len(reason) != 0:
          for line in reason.splitlines():
            out.write("     " + line + '\n')
            print "     " + line
          if not test.passed:
            out.write("     See %s\n" % specific_rules_url)
            print "     See %s\n" % specific_rules_url

      [output_result(r1,r2,url) for (r1, r2, url) in results]

      if test.failed:
        self.benchmarker.report_verify_results(self, test_type, 'fail')
      elif test.warned:
        self.benchmarker.report_verify_results(self, test_type, 'warn')
      elif test.passed:
        self.benchmarker.report_verify_results(self, test_type, 'pass')
      else:
        raise Exception("Unknown error - test did not pass,warn,or fail")

    result = True
    for test_type in self.runTests:
      verify_type(test_type)
      if self.runTests[test_type].failed:
        result = False
    
    return result
  ############################################################
  # End verify_urls
  ############################################################

  ############################################################
  # benchmark
  # Runs the benchmark for each type of test that it implements
  # JSON/DB/Query.
  ############################################################
  def benchmark(self, out, err):

    def benchmark_type(test_type):  
      out.write("BENCHMARKING %s ... " % test_type.upper())

      test = self.runTests[test_type]
      test.setup_out_err(out, err)
      output_file = self.benchmarker.output_file(self.name, test_type)
      if not os.path.exists(output_file):
        # Open to create the empty file
        with open(output_file, 'w'):
          pass

      if not test.failed:
        if test_type == 'plaintext': # One special case
          remote_script = self.__generate_pipeline_script(test.get_url(), self.port, test.accept_header)
        elif test_type == 'query' or test_type == 'update':
          remote_script = self.__generate_query_script(test.get_url(), self.port, test.accept_header)
        else:
          remote_script = self.__generate_concurrency_script(test.get_url(), self.port, test.accept_header)
        
        # Begin resource usage metrics collection
        self.__begin_logging(test_type)
        
        # Run the benchmark 
        with open(output_file, 'w') as raw_file:
          p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE, stdout=raw_file, stderr=err)
          p.communicate(remote_script)
          err.flush()

        # End resource usage metrics collection
        self.__end_logging()

      results = self.__parse_test(test_type)
      print "Benchmark results:"
      pprint(results)

      self.benchmarker.report_benchmark_results(framework=self, test=test_type, results=results['results'])
      out.write( "Complete\n" )
      out.flush()
    
    for test_type in self.runTests:
      benchmark_type(test_type)
  ############################################################
  # End benchmark
  ############################################################
  
  ############################################################
  # parse_all
  # Method meant to be run for a given timestamp
  ############################################################
  def parse_all(self):
    for test_type in self.runTests:
      if os.path.exists(self.benchmarker.get_output_file(self.name, test_type)):
        results = self.__parse_test(test_type)
        self.benchmarker.report_benchmark_results(framework=self, test=test_type, results=results['results'])

  ##########################################################################################
  # Private Methods
  ##########################################################################################

  ############################################################
  # __parse_test(test_type)
  ############################################################
  def __parse_test(self, test_type):
    try:
      results = dict()
      results['results'] = []
      stats = []
      
      if os.path.exists(self.benchmarker.get_output_file(self.name, test_type)):
        with open(self.benchmarker.output_file(self.name, test_type)) as raw_data:
          is_warmup = True
          rawData = None
          for line in raw_data:

            if "Queries:" in line or "Concurrency:" in line:
              is_warmup = False
              rawData = None
              continue
            if "Warmup" in line or "Primer" in line:
              is_warmup = True
              continue

            if not is_warmup:
              if rawData == None:
                rawData = dict()
                results['results'].append(rawData)

              #if "Requests/sec:" in line:
              #  m = re.search("Requests/sec:\s+([0-9]+)", line)
              #  rawData['reportedResults'] = m.group(1)
                
              # search for weighttp data such as succeeded and failed.
              if "Latency" in line:
                m = re.findall("([0-9]+\.*[0-9]*[us|ms|s|m|%]+)", line)
                if len(m) == 4:
                  rawData['latencyAvg'] = m[0]
                  rawData['latencyStdev'] = m[1]
                  rawData['latencyMax'] = m[2]
              #    rawData['latencyStdevPercent'] = m[3]
              
              #if "Req/Sec" in line:
              #  m = re.findall("([0-9]+\.*[0-9]*[k|%]*)", line)
              #  if len(m) == 4:
              #    rawData['requestsAvg'] = m[0]
              #    rawData['requestsStdev'] = m[1]
              #    rawData['requestsMax'] = m[2]
              #    rawData['requestsStdevPercent'] = m[3]
                
              #if "requests in" in line:
              #  m = re.search("requests in ([0-9]+\.*[0-9]*[ms|s|m|h]+)", line)
              #  if m != None: 
              #    # parse out the raw time, which may be in minutes or seconds
              #    raw_time = m.group(1)
              #    if "ms" in raw_time:
              #      rawData['total_time'] = float(raw_time[:len(raw_time)-2]) / 1000.0
              #    elif "s" in raw_time:
              #      rawData['total_time'] = float(raw_time[:len(raw_time)-1])
              #    elif "m" in raw_time:
              #      rawData['total_time'] = float(raw_time[:len(raw_time)-1]) * 60.0
              #    elif "h" in raw_time:
              #      rawData['total_time'] = float(raw_time[:len(raw_time)-1]) * 3600.0
             
              if "requests in" in line:
                m = re.search("([0-9]+) requests in", line)
                if m != None: 
                  rawData['totalRequests'] = int(m.group(1))
              
              if "Socket errors" in line:
                if "connect" in line:
                  m = re.search("connect ([0-9]+)", line)
                  rawData['connect'] = int(m.group(1))
                if "read" in line:
                  m = re.search("read ([0-9]+)", line)
                  rawData['read'] = int(m.group(1))
                if "write" in line:
                  m = re.search("write ([0-9]+)", line)
                  rawData['write'] = int(m.group(1))
                if "timeout" in line:
                  m = re.search("timeout ([0-9]+)", line)
                  rawData['timeout'] = int(m.group(1))
              
              if "Non-2xx" in line:
                m = re.search("Non-2xx or 3xx responses: ([0-9]+)", line)
                if m != None: 
                  rawData['5xx'] = int(m.group(1))
              if "STARTTIME" in line:
                m = re.search("[0-9]+", line)
                rawData["startTime"] = int(m.group(0))
              if "ENDTIME" in line:
                m = re.search("[0-9]+", line)
                rawData["endTime"] = int(m.group(0))
                test_stats = self.__parse_stats(test_type, rawData["startTime"], rawData["endTime"], 1)
                # rawData["averageStats"] = self.__calculate_average_stats(test_stats)
                stats.append(test_stats)
      with open(self.benchmarker.stats_file(self.name, test_type) + ".json", "w") as stats_file:
        json.dump(stats, stats_file, indent=2)


      return results
    except IOError:
      return None
  ############################################################
  # End benchmark
  ############################################################

  ############################################################
  # __generate_concurrency_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable concurrency tests (JSON
  # and DB)
  ############################################################
  def __generate_concurrency_script(self, url, port, accept_header, wrk_command="wrk"):
    headers = self.headers_template.format(accept=accept_header)
    return self.concurrency_template.format(max_concurrency=max(self.benchmarker.concurrency_levels), 
      max_threads=self.benchmarker.threads, name=self.name, duration=self.benchmarker.duration, 
      levels=" ".join("{}".format(item) for item in self.benchmarker.concurrency_levels), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers, wrk=wrk_command)

  ############################################################
  # __generate_pipeline_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single pipeline test.
  ############################################################
  def __generate_pipeline_script(self, url, port, accept_header, wrk_command="wrk"):
    headers = self.headers_template.format(accept=accept_header)
    return self.pipeline_template.format(max_concurrency=16384, 
      max_threads=self.benchmarker.threads, name=self.name, duration=self.benchmarker.duration, 
      levels=" ".join("{}".format(item) for item in [256,1024,4096,16384]), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers, wrk=wrk_command,
      pipeline=16)

  ############################################################
  # __generate_query_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable query tests (Query)
  ############################################################
  def __generate_query_script(self, url, port, accept_header):
    headers = self.headers_template.format(accept=accept_header)
    return self.query_template.format(max_concurrency=max(self.benchmarker.concurrency_levels), 
      max_threads=self.benchmarker.threads, name=self.name, duration=self.benchmarker.duration, 
      levels=" ".join("{}".format(item) for item in self.benchmarker.query_levels), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers)

  ############################################################
  # Returns True if any test type this this framework test will use a DB
  ############################################################
  def requires_database(self):
    '''Returns True/False if this test requires a database'''
    return any(tobj.requires_db for (ttype,tobj) in self.runTests.iteritems())

  ############################################################
  # __begin_logging
  # Starts a thread to monitor the resource usage, to be synced with the client's time
  # TODO: MySQL and InnoDB are possible. Figure out how to implement them.
  ############################################################
  def __begin_logging(self, test_type):
    output_file = "{file_name}".format(file_name=self.benchmarker.get_stats_file(self.name, test_type))
    dstat_string = "dstat -afilmprsT --aio --fs --ipc --lock --raw --socket --tcp \
                                      --raw --socket --tcp --udp --unix --vm --disk-util \
                                      --rpc --rpcd --output {output_file}".format(output_file=output_file)
    cmd = shlex.split(dstat_string)
    dev_null = open(os.devnull, "w")
    self.subprocess_handle = subprocess.Popen(cmd, stdout=dev_null)

  ##############################################################
  # Begin __end_logging
  # Stops the logger thread and blocks until shutdown is complete. 
  ##############################################################
  def __end_logging(self):
    self.subprocess_handle.terminate()
    self.subprocess_handle.communicate()

  ##############################################################
  # Begin __parse_stats
  # For each test type, process all the statistics, and return a multi-layered dictionary
  # that has a structure as follows:
  # (timestamp)
  # | (main header) - group that the stat is in
  # | | (sub header) - title of the stat
  # | | | (stat) - the stat itself, usually a floating point number
  ##############################################################
  def __parse_stats(self, test_type, start_time, end_time, interval):
    stats_dict = dict()
    stats_file = self.benchmarker.stats_file(self.name, test_type)
    with open(stats_file) as stats:
      while(stats.next() != "\n"): # dstat doesn't output a completely compliant CSV file - we need to strip the header
        pass
      stats_reader = csv.reader(stats)
      main_header = stats_reader.next()
      sub_header = stats_reader.next()
      time_row = sub_header.index("epoch")
      int_counter = 0
      for row in stats_reader:
        time = float(row[time_row])
        int_counter+=1
        if time < start_time:
          continue
        elif time > end_time:
          return stats_dict
        if int_counter % interval != 0:
          continue
        row_dict = dict()
        for nextheader in main_header:
          if nextheader != "":
            row_dict[nextheader] = dict()
        header = ""
        for item_num, column in enumerate(row):
          if(len(main_header[item_num]) != 0):
            header = main_header[item_num]
          row_dict[header][sub_header[item_num]] = float(column) # all the stats are numbers, so we want to make sure that they stay that way in json
        stats_dict[time] = row_dict
    return stats_dict
  ##############################################################
  # End __parse_stats
  ##############################################################

  def __getattr__(self, name):
    """For backwards compatibility, we used to pass benchmarker 
    as the argument to the setup.sh files"""
    try:
      x = getattr(self.benchmarker, name)
    except AttributeError:
      print "AttributeError: %s not a member of FrameworkTest or Benchmarker" % name
      print "This is probably a bug"
      raise
    return x

  ##############################################################
  # Begin __calculate_average_stats
  # We have a large amount of raw data for the statistics that
  # may be useful for the stats nerds, but most people care about
  # a couple of numbers. For now, we're only going to supply:
  # * Average CPU
  # * Average Memory
  # * Total network use
  # * Total disk use
  # More may be added in the future. If they are, please update
  # the above list.
  # Note: raw_stats is directly from the __parse_stats method.
  # Recall that this consists of a dictionary of timestamps, 
  # each of which contain a dictionary of stat categories which
  # contain a dictionary of stats
  ##############################################################
  def __calculate_average_stats(self, raw_stats):
    raw_stat_collection = dict()
    
    for timestamp, time_dict in raw_stats.items():
      for main_header, sub_headers in time_dict.items():
        item_to_append = None
        if 'cpu' in main_header:
          # We want to take the idl stat and subtract it from 100
          # to get the time that the CPU is NOT idle.
          item_to_append = sub_headers['idl'] - 100.0
        elif main_header == 'memory usage':
          item_to_append = sub_headers['used']
        elif 'net' in main_header:
          # Network stats have two parts - recieve and send. We'll use a tuple of
          # style (recieve, send)
          item_to_append = (sub_headers['recv'], sub_headers['send'])
        elif 'dsk' or 'io' in main_header:
          # Similar for network, except our tuple looks like (read, write)
          item_to_append = (sub_headers['read'], sub_headers['writ'])
        if item_to_append is not None:
          if main_header not in raw_stat_collection:
            raw_stat_collection[main_header] = list()
          raw_stat_collection[main_header].append(item_to_append)

    # Simple function to determine human readable size
    # http://stackoverflow.com/questions/1094841/reusable-library-to-get-human-readable-version-of-file-size
    def sizeof_fmt(num):
      # We'll assume that any number we get is convertable to a float, just in case
      num = float(num)
      for x in ['bytes','KB','MB','GB']:
        if num < 1024.0 and num > -1024.0:
          return "%3.1f%s" % (num, x)
        num /= 1024.0
      return "%3.1f%s" % (num, 'TB')

    # Now we have our raw stats in a readable format - we need to format it for display
    # We need a floating point sum, so the built in sum doesn't cut it
    display_stat_collection = dict()
    for header, values in raw_stat_collection.items():
      display_stat = None
      if 'cpu' in header:
        display_stat = sizeof_fmt(math.fsum(values) / len(values))
      elif main_header == 'memory usage':
        display_stat = sizeof_fmt(math.fsum(values) / len(values))
      elif 'net' in main_header:
        receive, send = zip(*values) # unzip
        display_stat = {'receive': sizeof_fmt(math.fsum(receive)), 'send': sizeof_fmt(math.fsum(send))}
      else: # if 'dsk' or 'io' in header:
        read, write = zip(*values) # unzip
        display_stat = {'read': sizeof_fmt(math.fsum(read)), 'write': sizeof_fmt(math.fsum(write))}
      display_stat_collection[header] = display_stat
    return display_stat
  ###########################################################################################
  # End __calculate_average_stats
  #########################################################################################

          
  ##########################################################################################
  # Constructor
  ##########################################################################################  
  def __init__(self, name, directory, benchmarker, runTests, args):
    self.name = name
    self.directory = directory
    self.benchmarker = benchmarker
    self.runTests = runTests
    self.fwroot = benchmarker.fwroot
    self.approach = ""
    self.classification = ""
    self.database = ""
    self.framework = ""
    self.language = ""
    self.orm = ""
    self.platform = ""
    self.webserver = ""
    self.os = ""
    self.database_os = ""
    self.display_name = ""
    self.notes = ""
    self.versus = ""
    
    # setup logging
    logging.basicConfig(stream=sys.stderr, level=logging.INFO)
    
    self.install_root="%s/%s" % (self.fwroot, "installs")
    if benchmarker.install_strategy is 'pertest':
      self.install_root="%s/pertest/%s" % (self.install_root, name)

    # Used in setup.sh scripts for consistency with 
    # the bash environment variables
    self.troot = self.directory
    self.iroot = self.install_root

    self.__dict__.update(args)
  ############################################################
  # End __init__
  ############################################################
############################################################
# End FrameworkTest
############################################################

##########################################################################################
# Static methods
##########################################################################################

##############################################################
# parse_config(config, directory, benchmarker)
# parses a config file and returns a list of FrameworkTest
# objects based on that config file.
##############################################################
def parse_config(config, directory, benchmarker):
  tests = []

  # This sort ordering is set up specifically to return the length
  # of the test name. There were SO many problems involved with
  # 'plaintext' being run first (rather, just not last) that we
  # needed to ensure that it was run last for every framework.
  def testOrder(type_name):
    return len(type_name)

  # The config object can specify multiple tests
  #   Loop over them and parse each into a FrameworkTest
  for test in config['tests']:

    names = [name for (name,keys) in test.iteritems()]
    if "default" not in names:
      logging.warn("Framework %s does not define a default test in benchmark_config.json", config['framework'])
    
    # Check that each test configuration is acceptable
    # Throw exceptions if a field is missing, or how to improve the field
    for test_name, test_keys in test.iteritems():
      # Ensure that each FrameworkTest has a framework property, inheriting from top-level if not
      if not test_keys['framework']:
        test_keys['framework'] = config['framework']

      # Confirm required keys are present
      required_keys = ['language','webserver','classification','database','approach','orm','framework','os','database_os']
      missing = list(set(required_keys) - set(test_keys))
      if len(missing) > 0:
        missingstr = (", ").join(map(str, missing))
        raise Exception("benchmark_config.json for test %s is invalid, please amend and add the following required keys: [%s]"
          % (test_name, missingstr))
      
      # Check that test url values are all appropriate
      example_urls = {
        "json_url":      "/json",
        "db_url":        "/mysql/db",
        "query_url":     "/mysql/queries?queries=  or  /mysql/queries/",
        "fortune_url":   "/mysql/fortunes",
        "update_url":    "/mysql/updates?queries=  or  /mysql/updates/",
        "plaintext_url": "/plaintext"
      }
      for test_url in ["json_url","db_url","query_url","fortune_url","update_url","plaintext_url"]:
        key_value = test_keys.get(test_url, None)
        if key_value != None and not key_value.startswith('/'):
          errmsg = """`%s` field in test \"%s\" does not appear to be a valid url: \"%s\"\n
            Example `%s` url: \"%s\"
          """ % (test_url, test_name, key_value, test_url, example_urls[test_url])
          raise Exception(errmsg)

      # Check database type
      # List adopted from run-ci.py
      SUPPORTED_DATABASES = ["mysql","postgres","mongodb","cassandra","elasticsearch","redis"]
      EDGE_CASES = ["sqlite","sqlserver","none"]
      db_type = test_keys.get("database", None).lower()

      if db_type.lower() not in sum([SUPPORTED_DATABASES, EDGE_CASES], []):
        supportedstr = (", ").join(map(str, SUPPORTED_DATABASES))
        edgestr = (", ").join(map(str, EDGE_CASES))

        errmsg = """Invalid db specified for test \"%s\" in framework \"%s\", please specify a supported database or \"None\"\n
           Supported databases: [%s]\n
           Edge cases: [%s]\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], supportedstr, edgestr, db_type)
        raise Exception(errmsg)

      # Check language
      # "Scala" from "/home/vagrant/FrameworkBenchmarks/frameworks/Scala/finagle"
      recommended_lang = directory.split('/')[-2]
      
      if test_keys.get("language", "") == "":
        raise Exception("Please specify a language for test \"%s\" in framework \"%s\", suggestion: \"%s\""
          % (test_name, config["framework"], recommended_lang))

      # Check approach
      SUPPORTED_APPROACHES = ["realistic","stripped"]
      test_approach = test_keys.get("approach", None).lower()
      if test_approach not in SUPPORTED_APPROACHES:
        approachstr = (", ").join(map(str, SUPPORTED_APPROACHES))

        errmsg = """Invalid approach specified for test \"%s\" in framework \"%s\", please specify a supported approach\n
           Supported approaches: [%s]\n
           Suggestion: \"Realistic\"\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], approachstr, test_approach)
        raise Exception(errmsg)

      # Check classification
      SUPPORTED_CLASSIFICATIONS = ["fullstack","micro","platform"]
      test_classification = test_keys.get("classification", None).lower()
      if test_classification not in SUPPORTED_CLASSIFICATIONS:
        classstr = (", ").join(map(str, SUPPORTED_CLASSIFICATIONS))

        errmsg = """Invalid classification specified for test \"%s\" in framework \"%s\", please specify a supported classification\n
           Supported classifications: [%s]\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], classstr, test_classification)
        raise Exception(errmsg)

      # Check webserver
      if test_keys.get("webserver", None) == "":
        raise Exception("Invalid `webserver` specified for test \"%s\" in framework \"%s\", field `webserver` cannot be empty"
          % (test_name, config["framework"]))

      # Check ORM
      SUPPORTED_ORMS = ["full","micro","raw"]
      test_orm = test_keys.get("orm", None).lower()
      if test_orm not in SUPPORTED_ORMS:
        ormstr = (", ").join(map(str, SUPPORTED_ORMS))

        errmsg = """Invalid orm specified for test \"%s\" in framework \"%s\", please specify a supported orm type\n
           Supported classifications: [%s]\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], ormstr, test_orm)
        raise Exception(errmsg)

      # Check OS
      SUPPORTED_OSES = ["linux","windows"]
      test_os = test_keys.get("os", None).lower()
      if test_os not in SUPPORTED_OSES:
        osstr = (", ").join(map(str, SUPPORTED_OSES))

        errmsg = """Invalid OS specified for test \"%s\" in framework \"%s\", please specify a supported OS\n
           Supported OS's: [%s]\n
           Suggestion: \"Linux\"\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], osstr, test_os)
        raise Exception(errmsg)

      # Check Database OS
      SUPPORTED_DB_OSES = ["linux","windows"]
      test_db_os = test_keys.get("database_os", None).lower()
      if test_db_os not in SUPPORTED_DB_OSES:
        db_osstr = (", ").join(map(str, SUPPORTED_DB_OSES))

        errmsg = """Invalid Database OS specified for test \"%s\" in framework \"%s\", please specify a supported Database OS\n
           Supported OS's: [%s]\n
           Suggestion: \"Linux\"\n
           Supplied (lowercased): \"%s\"
        """ % (test_name, config["framework"], db_osstr, test_db_os)
        raise Exception(errmsg)

      ### Done validating benchmark_config values ###

      
      # Map test type to a parsed FrameworkTestType object
      runTests = dict()
      for type_name, type_obj in benchmarker.types.iteritems():
        try:
          runTests[type_name] = type_obj.copy().parse(test_keys)
        except AttributeError as ae:
          # This is quite common - most tests don't support all types
          # Quitely log it and move on (debug logging is on in travis and this causes 
          # ~1500 lines of debug, so I'm totally ignoring it for now
          # logging.debug("Missing arguments for test type %s for framework test %s", type_name, test_name)
          pass

      # We need to sort by test_type to run
      sortedTestKeys = sorted(runTests.keys(), key=testOrder)
      sortedRunTests = OrderedDict()
      for sortedTestKey in sortedTestKeys:
        sortedRunTests[sortedTestKey] = runTests[sortedTestKey]

      # Prefix all test names with framework except 'default' test
      # Done at the end so we may still refer to the primary test as `default` in benchmark config error messages
      if test_name == 'default': 
        test_name = config['framework']
      else:
        test_name = "%s-%s" % (config['framework'], test_name) 

      # By passing the entire set of keys, each FrameworkTest will have a member for each key
      tests.append(FrameworkTest(test_name, directory, benchmarker, sortedRunTests, test_keys))

  return tests
##############################################################
# End parse_config
##############################################################
