import importlib
import os
import subprocess
import time
import re
import pprint
import sys
import traceback
import psutil
import threading

class FrameworkTest:
  ##########################################################################################
  # Class variables
  ##########################################################################################
  headers_template = "-H 'Host: localhost' -H '{accept}' -H 'Connection: keep-alive'"
  headers_full_template = "-H 'Host: localhost' -H '{accept}' -H 'Accept-Language: en-US,en;q=0.5' -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20130501 Firefox/30.0 AppleWebKit/600.00 Chrome/30.0.0000.0 Trident/10.0 Safari/600.00' -H 'Cookie: uid=12345678901234567890; __utma=1.1234567890.1234567890.1234567890.1234567890.12; wd=2560x1600' -H 'Connection: keep-alive'"
 
  accept_json = "Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
  accept_html = "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  accept_plaintext = "Accept: text/plain,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"

  concurrency_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " {wrk} {headers} -d 5 -c 8 -t 8 \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} -d 5 -c 8 -t 8 "http://{server_host}:{port}{url}"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " {wrk} {headers} -d {duration} -c {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}\""
    echo "---------------------------------------------------------"
    echo ""
    {wrk} {headers} -d {duration} -c {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}"
    sleep 5
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Concurrency: $c for {name}"
      echo " {wrk} {headers} {pipeline} -d {duration} -c $c -t $(($c>{max_threads}?{max_threads}:$c)) \"http://{server_host}:{port}{url}\""
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} {pipeline} -d {duration} -c "$c" -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url}
      sleep 2
    done
  """

  query_template = """
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Primer {name}"
    echo " wrk {headers} -d 5 -c 8 -t 8 \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} -d 5 -c 8 -t 8 "http://{server_host}:{port}{url}2"
    sleep 5
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " wrk {headers} -d {duration} -c {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}2\""
    echo "---------------------------------------------------------"
    echo ""
    wrk {headers} -d {duration} -c {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}2"
    sleep 5
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Queries: $c for {name}"
      echo " wrk {headers} -d {duration} -c {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}$c\""
      echo "---------------------------------------------------------"
      echo ""
      wrk {headers} -d {duration} -c {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}$c"
      sleep 2
    done
  """

  language = None
  platform = None
  webserver = None
  classification = None
  database = None
  approach = None
  orm = None
  framework = None
  os = None
  database_os = None
  display_name = None
  notes = None
  versus = None

  ##########################################################################################
  # Public Methods
  ##########################################################################################

  ############################################################
  # start(benchmarker)
  # Start the test using it's setup file
  ############################################################
  def start(self, out, err):
    return self.setup_module.start(self.benchmarker, out, err)
  ############################################################
  # End start
  ############################################################

  ############################################################
  # stop(benchmarker)
  # Stops the test using it's setup file
  ############################################################
  def stop(self, out, err):
    return self.setup_module.stop(out, err)
  ############################################################
  # End stop
  ############################################################

  ############################################################
  # verify_urls
  # Verifys each of the URLs for this test. THis will sinply 
  # curl the URL and check for it's return status. 
  # For each url, a flag will be set on this object for whether
  # or not it passed
  ############################################################
  def verify_urls(self, out, err):
    # JSON
    try:
      out.write( "VERIFYING JSON (" + self.json_url + ") ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.json_url, self.port)
      self.__curl_url(url, out, err)
      self.json_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.json_url_passed = False

    # DB
    try:
      out.write( "VERIFYING DB (" + self.db_url + ") ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.db_url, self.port)
      self.__curl_url(url, out, err)
      self.db_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.db_url_passed = False

    # Query
    try:
      out.write( "VERIFYING Query (" + self.query_url + "2) ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.query_url + "2", self.port)
      self.__curl_url(url, out, err)
      self.query_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.query_url_passed = False

    # Fortune
    try:
      out.write( "VERIFYING Fortune (" + self.fortune_url + ") ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.fortune_url, self.port)
      self.__curl_url(url, out, err)
      self.fortune_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.fortune_url_passed = False

    # Update
    try:
      out.write( "VERIFYING Update (" + self.update_url + "2) ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.update_url + "2", self.port)
      self.__curl_url(url, out, err)
      self.update_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.update_url_passed = False

    # plaintext
    try:
      out.write( "VERIFYING Plaintext (" + self.plaintext_url + ") ...\n" )
      out.flush()
      url = self.benchmarker.generate_url(self.plaintext_url, self.port)
      self.__curl_url(url, out, err)
      self.plaintext_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.plaintext_url_passed = False
  ############################################################
  # End verify_urls
  ############################################################

  ############################################################
  # contains_type(type)
  # true if this test contains an implementation of the given 
  # test type (json, db, etc.)
  ############################################################
  def contains_type(self, type):
    try:
      if type == 'json' and self.json_url != None:
        return True
      if type == 'db' and self.db_url != None:
        return True
      if type == 'query' and self.query_url != None:
        return True
      if type == 'fortune' and self.fortune_url != None:
        return True
      if type == 'update' and self.update_url != None:
        return True
      if type == 'plaintext' and self.plaintext_url != None:
        return True
    except AttributeError:
      pass
      
    return False
  ############################################################
  # End stop
  ############################################################

  # List of snapshots of performance data
  stats_records = []

  # Stat baselines, to subtract from snapshots
  memory_baseline = 0
  net_baseline = dict()
  disk_baseline = dict()

  ############################################################
  # estimate_memory_usage
  # A best effort at calculating actual writable memory of
  # this user's processes
  ############################################################
  def estimate_memory_usage(self):
    if os.name == 'posix':
      # Linux case, via crawling /proc.
      # We're looking for private dirty memory, to avoid
      # double counting shared memory across processes or
      # other pitfalls.
      kbytes = subprocess.check_output("grep Private_Dirty /proc/*/smaps 2>/dev/null | sed -e 's/.*\\([0-9][0-9]*\\).*/\\1/g' | awk '{s+=$1} END {print s}'", shell=True)
      return int(kbytes)
    else:
      # Punting on other OSes for now!
      return 0

  ############################################################
  # init_stats
  # Initializes state for capturing performance numbers.
  ############################################################
  def init_stats(self):
    self.stats_records = []
    self.net_baseline = psutil.network_io_counters()
    self.disk_baseline = psutil.disk_io_counters()

  ############################################################
  # grab_memory_baseline
  # Call this before actually running the framework,
  # to record how much memory other processes were using.
  ############################################################
  def grab_memory_baseline(self):
    self.memory_baseline = self.estimate_memory_usage()

  ############################################################
  # stats_snapshot
  # Save one snapshot of performance numbers.
  ############################################################
  def stats_snapshot(self):
    snap = dict()
    snap['cpu'] = psutil.cpu_percent(interval=1)
    snap['mem'] = self.estimate_memory_usage() - self.memory_baseline

    net = psutil.network_io_counters()
    netd = dict()
    netd['sent'] = net.bytes_sent - self.net_baseline.bytes_sent
    netd['recv'] = net.bytes_recv - self.net_baseline.bytes_recv
    snap['net'] = netd

    disk = psutil.disk_io_counters()
    diskd = dict()
    diskd['read'] = disk.read_bytes - self.disk_baseline.read_bytes
    diskd['write'] = disk.write_bytes - self.disk_baseline.write_bytes
    snap['disk'] = diskd

    self.stats_records.append(snap)

  ############################################################
  # statsThread
  # ...which captures the performance numbers
  ############################################################
  class statsThread (threading.Thread):
    def __init__(self, parent, intervals):
      threading.Thread.__init__(self)
      self.intervals = intervals
      self.parent = parent
    def run(self):
      for interval in self.intervals:
        time.sleep(interval)
        self.parent.stats_snapshot()

  ############################################################
  # start_stats
  # Start capturing performance numbers, running one capture
  # after each seconds time interval in the argument.
  ############################################################
  def start_stats(self, intervals):
    self.init_stats()
    self.statsThread(self, intervals).start()

  ############################################################
  # placeholder_stats
  # This function should be rewritten to follow a more
  # reasoned distribution of samples across time!
  ############################################################
  def placeholder_stats(self):
    self.start_stats([15, 30])

  ############################################################
  # benchmark
  # Runs the benchmark for each type of test that it implements
  # JSON/DB/Query.
  ############################################################
  def benchmark(self, out, err):
    # JSON
    try:
      if self.json_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "json"):
        out.write("BENCHMARKING JSON ... ") 
        out.flush()
        remote_script = self.__generate_concurrency_script(self.json_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'json'), err)
        results = self.__parse_test('json')
        self.benchmarker.report_results(framework=self, test="json", results=results['results'])
        out.write( "Complete\n" )
        out.flush()
    except AttributeError:
      pass

    # DB
    try:
      if self.db_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "db"):
        out.write("BENCHMARKING DB ... ") 
        out.flush()
        remote_script = self.__generate_concurrency_script(self.db_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'db'), err)
        results = self.__parse_test('db')
        self.benchmarker.report_results(framework=self, test="db", results=results['results'])
        out.write( "Complete\n" )
    except AttributeError:
      traceback.print_exc()
      pass

    # Query
    try:
      if self.query_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "query"):
        out.write("BENCHMARKING Query ... ")
        out.flush()
        remote_script = self.__generate_query_script(self.query_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'query'), err)
        results = self.__parse_test('query')
        self.benchmarker.report_results(framework=self, test="query", results=results['results'])
        out.write( "Complete\n" )
        out.flush()
    except AttributeError:
      traceback.print_exc()
      pass

    # fortune
    try:
      if self.fortune_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "fortune"):
        out.write("BENCHMARKING Fortune ... ") 
        out.flush()
        remote_script = self.__generate_concurrency_script(self.fortune_url, self.port, self.accept_html)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'fortune'), err)
        results = self.__parse_test('fortune')
        self.benchmarker.report_results(framework=self, test="fortune", results=results['results'])
        out.write( "Complete\n" )
        out.flush()
    except AttributeError:
      traceback.print_exc()
      pass

    # update
    try:
      if self.update_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "update"):
        out.write("BENCHMARKING Update ... ") 
        out.flush()
        remote_script = self.__generate_query_script(self.update_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'update'), err)
        results = self.__parse_test('update')
        self.benchmarker.report_results(framework=self, test="update", results=results['results'])
        out.write( "Complete\n" )
        out.flush()
    except AttributeError:
      # TODO - this needs to report some logging
      traceback.print_exc()
      pass

    # plaintext
    try:
      if self.plaintext_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "plaintext"):
        out.write("BENCHMARKING Plaintext ... ")
        out.flush()
        remote_script = self.__generate_concurrency_script(self.plaintext_url, self.port, self.accept_plaintext, wrk_command="wrk-pipeline", intervals=[256,1024,4096,16384], pipeline="--pipeline 16")
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'plaintext'), err)
        results = self.__parse_test('plaintext')
        self.benchmarker.report_results(framework=self, test="plaintext", results=results['results'])
        out.write( "Complete\n" )
        out.flush()
    except AttributeError:
      traceback.print_exc()
      pass
  ############################################################
  # End benchmark
  ############################################################
  
  ############################################################
  # parse_all
  # Method meant to be run for a given timestamp
  ############################################################
  def parse_all(self):
    # JSON
    if os.path.exists(self.benchmarker.output_file(self.name, 'json')):
      results = self.__parse_test('json')
      self.benchmarker.report_results(framework=self, test="json", results=results['results'])
    
    # DB
    if os.path.exists(self.benchmarker.output_file(self.name, 'db')):
      results = self.__parse_test('db')
      self.benchmarker.report_results(framework=self, test="db", results=results['results'])
    
    # Query
    if os.path.exists(self.benchmarker.output_file(self.name, 'query')):
      results = self.__parse_test('query')
      self.benchmarker.report_results(framework=self, test="query", results=results['results'])

    # Fortune
    if os.path.exists(self.benchmarker.output_file(self.name, 'fortune')):
      results = self.__parse_test('fortune')
      self.benchmarker.report_results(framework=self, test="fortune", results=results['results'])

    # Update
    if os.path.exists(self.benchmarker.output_file(self.name, 'update')):
      results = self.__parse_test('update')
      self.benchmarker.report_results(framework=self, test="update", results=results['results'])

    # Plaintext
    if os.path.exists(self.benchmarker.output_file(self.name, 'plaintext')):
      results = self.__parse_test('plaintext')
      self.benchmarker.report_results(framework=self, test="plaintext", results=results['results'])
  ############################################################
  # End parse_all
  ############################################################

  ############################################################
  # __parse_test(test_type)
  ############################################################
  def __parse_test(self, test_type):
    try:
      results = dict()
      results['results'] = dict()
      results['results']['wrk'] = []
      results['results']['snapshots'] = self.stats_records
      
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
              results['results']['wrk'].append(rawData)

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
  # __run_benchmark(script, output_file)
  # Runs a single benchmark using the script which is a bash 
  # template that uses weighttp to run the test. All the results
  # outputed to the output_file.
  ############################################################
  def __run_benchmark(self, script, output_file, err):
    self.placeholder_stats()

    with open(output_file, 'w') as raw_file:
	  
      p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE, stdout=raw_file, stderr=err)
      p.communicate(script)
      err.flush()

    self.stats_snapshot()
  ############################################################
  # End __run_benchmark
  ############################################################

  ############################################################
  # __generate_concurrency_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable concurrency tests (JSON
  # and DB)
  ############################################################
  def __generate_concurrency_script(self, url, port, accept_header, wrk_command="wrk", intervals=[], pipeline=""):
    if len(intervals) == 0:
      intervals = self.benchmarker.concurrency_levels
    headers = self.__get_request_headers(accept_header)
    return self.concurrency_template.format(max_concurrency=self.benchmarker.max_concurrency, 
      max_threads=self.benchmarker.max_threads, name=self.name, duration=self.benchmarker.duration, 
      interval=" ".join("{}".format(item) for item in intervals), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers, wrk=wrk_command,
      pipeline=pipeline)
  ############################################################
  # End __generate_concurrency_script
  ############################################################

  ############################################################
  # __generate_query_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable query tests (Query)
  ############################################################
  def __generate_query_script(self, url, port, accept_header):
    headers = self.__get_request_headers(accept_header)
    return self.query_template.format(max_concurrency=self.benchmarker.max_concurrency, 
      max_threads=self.benchmarker.max_threads, name=self.name, duration=self.benchmarker.duration, 
      interval=" ".join("{}".format(item) for item in self.benchmarker.query_intervals), 
      server_host=self.benchmarker.server_host, port=port, url=url, headers=headers)
  ############################################################
  # End __generate_query_script
  ############################################################

  ############################################################
  # __get_request_headers(accept_header)
  # Generates the complete HTTP header string
  ############################################################
  def __get_request_headers(self, accept_header):
    return self.headers_template.format(accept=accept_header)
  ############################################################
  # End __format_request_headers
  ############################################################

  ############################################################
  # __curl_url
  # Dump HTTP response and headers. Throw exception if there
  # is an HTTP error.
  ############################################################
  def __curl_url(self, url, out, err):
    # Use -i to output response with headers.
    # Don't use -f so that the HTTP response code is ignored.
    # Use --stderr - to redirect stderr to stdout so we get
    # error output for sure in stdout.
    # Use -sS to hide progress bar, but show errors.
    subprocess.check_call(["curl", "-i", "-sS", url], stderr=err, stdout=out)
    out.flush()
    err.flush()
    # HTTP output may not end in a newline, so add that here.
    out.write( "\n" )
    out.flush()
    # In the curl invocation above we could not use -f because
    # then the HTTP response would not be output, so use -f in
    # an additional invocation so that if there is an HTTP error,
    # subprocess.CalledProcessError will be thrown. Note that this
    # uses check_output() instead of check_call() so that we can
    # ignore the HTTP response because we already output that in
    # the first curl invocation.
    subprocess.check_output(["curl", "-fsS", url], stderr=err)
    out.flush()
    err.flush()
    # HTTP output may not end in a newline, so add that here.
    out.write( "\n" )
    out.flush()
  ##############################################################
  # End __curl_url
  ##############################################################

  ##########################################################################################
  # Constructor
  ##########################################################################################  
  def __init__(self, name, directory, benchmarker, args):
    self.name = name
    self.directory = directory
    self.benchmarker = benchmarker
    self.__dict__.update(args)

    # ensure directory has __init__.py file so that we can use it as a Python package
    if not os.path.exists(os.path.join(directory, "__init__.py")):
      open(os.path.join(directory, "__init__.py"), 'w').close()

    self.setup_module = setup_module = importlib.import_module(directory + '.' + self.setup_file)

    self.grab_memory_baseline()
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

  # The config object can specify multiple tests, we neep to loop
  # over them and parse them out
  for test in config['tests']:
    for key, value in test.iteritems():
      test_name = config['framework']
      
      # if the test uses the 'defualt' keywork, then we don't 
      # append anything to it's name. All configs should only have 1 default
      if key != 'default':
        # we need to use the key in the test_name
        test_name = test_name + "-" + key

      tests.append(FrameworkTest(test_name, directory, benchmarker, value))

  return tests
##############################################################
# End parse_config
##############################################################
