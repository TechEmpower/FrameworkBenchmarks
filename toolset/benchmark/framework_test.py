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
from threading import Thread
from threading import Event

from utils import header

class FrameworkTest:
  headers_template = "-H 'Host: localhost' -H '{accept}' -H 'Connection: keep-alive'"
 
  # Used for test types that do not require a database - 
  # These tests are run at multiple concurrency levels
  concurrency_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " {wrk} {headers} -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " {wrk} {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}"
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
      echo " {wrk} {headers} -d {duration} -c $c --timeout $c -t $(($c>{max_threads}?{max_threads}:$c)) \"http://{server_host}:{port}{url}\" -s ~/pipeline.lua -- {pipeline}"
      echo "---------------------------------------------------------"
      echo ""
      STARTTIME=$(date +"%s")
      {wrk} {headers} -d {duration} -c $c --timeout $c -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url} -s ~/pipeline.lua -- {pipeline}
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
    echo " wrk {headers} -d 5 -c 8 --timeout 8 -t 8 \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} -d 5 -c 8 --timeout 8 -t 8 "http://{server_host}:{port}{url}2"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}2"
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
      echo " wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}$c\""
      echo "---------------------------------------------------------"
      echo ""
      STARTTIME=$(date +"%s")
      wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}$c"
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
    # Load profile for this installation
    profile="%s/bash_profile.sh" % self.directory
    if not os.path.exists(profile):
      logging.warning("Directory %s does not have a bash_profile.sh" % self.directory)
      profile="$FWROOT/config/benchmark_profile"

    # Setup variables for TROOT and IROOT
    setup_util.replace_environ(config=profile, 
              command='export TROOT=%s && export IROOT=%s' %
              (self.directory, self.install_root))

    # Because start can take so long, we print a dot to let the user know 
    # we are working
    class ProgressPrinterThread(Thread):
      def __init__(self, event):
          Thread.__init__(self)
          self.stopped = event

      def run(self):
        while not self.stopped.wait(20):
          sys.stderr.write("Waiting for start to return...\n")
    stopFlag = Event()
    thread = ProgressPrinterThread(stopFlag)
    thread.start()

    # Run the module start (inside parent of TROOT)
    #     - we use the parent as a historical accident - a lot of tests
    #       use subprocess's cwd argument already
    previousDir = os.getcwd()
    os.chdir(os.path.dirname(self.troot))
    logging.info("Running setup module start (cwd=%s)", os.path.dirname(self.troot))
    try:
      retcode = self.setup_module.start(self, out, err)    
      if retcode == None: 
        retcode = 0
    except Exception:
      retcode = 1
      st = traceback.format_exc()
      st = '\n'.join((4 * ' ') + x for x in st.splitlines())
      st = "Start exception:\n%s" % st
      logging.info(st)
      err.write(st + '\n')
    os.chdir(previousDir)

    # Stop the progress printer
    stopFlag.set()

    logging.info("Called setup.py start")

    return retcode
  ############################################################
  # End start
  ############################################################

  ############################################################
  # stop(benchmarker)
  # Stops the test using it's setup file
  ############################################################
  def stop(self, out, err):
    # Load profile for this installation
    profile="%s/bash_profile.sh" % self.directory
    if not os.path.exists(profile):
      logging.warning("Directory %s does not have a bash_profile.sh" % self.directory)
      profile="$FWROOT/config/benchmark_profile"
    
    setup_util.replace_environ(config=profile, 
              command='export TROOT=%s && export IROOT=%s' %
              (self.directory, self.install_root))

    # Run the module stop (inside parent of TROOT)
    #     - we use the parent as a historical accident - a lot of tests
    #       use subprocess's cwd argument already
    previousDir = os.getcwd()
    os.chdir(os.path.dirname(self.troot))
    logging.info("Running setup module stop (cwd=%s)", os.path.dirname(self.troot))
    try:
      retcode = self.setup_module.stop(out, err)
      if retcode == None: 
        retcode = 0
    except Exception:
      retcode = 1 
      st = traceback.format_exc()
      st = '\n'.join((4 * ' ') + x for x in st.splitlines())
      st = "Stop exception:\n%s\n" % st
      logging.info(st)
      err.write(st + '\n')
    os.chdir(previousDir)

    # Give processes sent a SIGTERM a moment to shut down gracefully
    time.sleep(5)

    return retcode
  ############################################################
  # End stop
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
        out.write("   %s for %s\n" % (result.upper(), url))
        print "   %s for %s" % (result.upper(), url)
        if reason is not None and len(reason) != 0:
          for line in reason.splitlines():
            out.write("     " + line + '\n')
            print "     " + line

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

      if test.passed:
        if test_type == 'plaintext': # One special case
          remote_script = self.__generate_concurrency_script(test.get_url(), self.port, test.accept_header, levels=[256,1024,4096,16384], pipeline="16")
        elif test.requires_db:
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

  ##########################################################################################
  # Private Methods
  ##########################################################################################

  ############################################################
  # __generate_concurrency_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable concurrency tests (JSON
  # and DB)
  ############################################################
  def __generate_concurrency_script(self, url, port, accept_header, wrk_command="wrk", levels=[], pipeline=""):
    if len(levels) == 0:
      levels = self.benchmarker.concurrency_levels
    headers = self.headers_template.format(accept=accept_header)
    return self.concurrency_template.format(max_concurrency=max(self.benchmarker.concurrency_levels), 
      max_threads=self.benchmarker.threads, name=self.name, duration=self.benchmarker.duration, 
      levels=" ".join("{}".format(item) for item in levels), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers, wrk=wrk_command,
      pipeline=pipeline)

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
    as the argument to the setup.py files"""
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
    
    # setup logging
    logging.basicConfig(stream=sys.stderr, level=logging.INFO)
    
    self.install_root="%s/%s" % (self.fwroot, "installs")
    if benchmarker.install_strategy is 'pertest':
      self.install_root="%s/pertest/%s" % (self.install_root, name)

    # Used in setup.py scripts for consistency with 
    # the bash environment variables
    self.troot = self.directory
    self.iroot = self.install_root

    self.__dict__.update(args)

    # ensure directory has __init__.py file so that we can use it as a Python package
    if not os.path.exists(os.path.join(directory, "__init__.py")):
      logging.warning("Please add an empty __init__.py file to directory %s", directory)
      open(os.path.join(directory, "__init__.py"), 'w').close()

    # Import the module (TODO - consider using sys.meta_path)
    # Note: You can see the log output if you really want to, but it's a *ton*
    dir_rel_to_fwroot = os.path.relpath(os.path.dirname(directory), self.fwroot)
    if dir_rel_to_fwroot != ".":
      sys.path.append("%s/%s" % (self.fwroot, dir_rel_to_fwroot))
      logging.log(0, "Adding %s to import %s.%s", dir_rel_to_fwroot, os.path.basename(directory), self.setup_file)
      self.setup_module = setup_module = importlib.import_module(os.path.basename(directory) + '.' + self.setup_file)
      sys.path.remove("%s/%s" % (self.fwroot, dir_rel_to_fwroot))
    else:
      logging.log(0, "Importing %s.%s", directory, self.setup_file)
      self.setup_module = setup_module = importlib.import_module(os.path.basename(directory) + '.' + self.setup_file)
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

  # The config object can specify multiple tests
  #   Loop over them and parse each into a FrameworkTest
  for test in config['tests']:
    for test_name, test_keys in test.iteritems():
      # Prefix all test names with framework except 'default' test
      if test_name == 'default': 
        test_name = config['framework']
      else:
        test_name = "%s-%s" % (config['framework'], test_name)  

      # Ensure FrameworkTest.framework is available
      if not test_keys['framework']:
        test_keys['framework'] = config['framework']
      #if test_keys['framework'].lower() != config['framework'].lower():
      #  print Exception("benchmark_config for test %s is invalid - test framework '%s' must match benchmark_config framework '%s'" % 
      #    (test_name, test_keys['framework'], config['framework']))

      # Confirm required keys are present
      # TODO have a TechEmpower person confirm this list - I don't know what the website requires....
      required = ['language','webserver','classification','database','approach','orm','framework','os','database_os']
      if not all (key in test_keys for key in required):
        raise Exception("benchmark_config for test %s is invalid - missing required keys" % test_name)      
      
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

      # By passing the entire set of keys, each FrameworkTest will have a member for each key
      tests.append(FrameworkTest(test_name, directory, benchmarker, runTests, test_keys))

  return tests
##############################################################
# End parse_config
##############################################################
