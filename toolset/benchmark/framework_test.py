import importlib
import os
import subprocess
import time
import re
import pprint
import sys

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
    echo " {wrk} {headers} -d 60 -c 8 -t 8 \"http://{server_host}:{port}{url}\""
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
  def start(self):
    return self.setup_module.start(self.benchmarker)
  ############################################################
  # End start
  ############################################################

  ############################################################
  # stop(benchmarker)
  # Stops the test using it's setup file
  ############################################################
  def stop(self):
    return self.setup_module.stop()
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
  def verify_urls(self):
    # JSON
    try:
      print "VERIFYING JSON (" + self.json_url + ") ..."
      url = self.benchmarker.generate_url(self.json_url, self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
      self.json_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.json_url_passed = False

    # DB
    try:
      print "VERIFYING DB (" + self.db_url + ") ..."
      url = self.benchmarker.generate_url(self.db_url, self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
      self.db_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.db_url_passed = False

    # Query
    try:
      print "VERIFYING Query (" + self.query_url + "2) ..."
      url = self.benchmarker.generate_url(self.query_url + "2", self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
      self.query_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.query_url_passed = False

    # Fortune
    try:
      print "VERIFYING Fortune (" + self.fortune_url + ") ..."
      url = self.benchmarker.generate_url(self.fortune_url, self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
      self.fortune_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.fortune_url_passed = False

    # Update
    try:
      print "VERIFYING Update (" + self.update_url + "2) ..."
      url = self.benchmarker.generate_url(self.update_url + "2", self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
      self.update_url_passed = True
    except (AttributeError, subprocess.CalledProcessError) as e:
      self.update_url_passed = False

    # plaintext
    try:
      print "VERIFYING Plaintext (" + self.plaintext_url + ") ..."
      url = self.benchmarker.generate_url(self.plaintext_url, self.port)
      subprocess.check_call(["curl", "-f", url])
      print ""
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

  ############################################################
  # benchmark
  # Runs the benchmark for each type of test that it implements
  # JSON/DB/Query.
  ############################################################
  def benchmark(self):
    # JSON
    try:
      if self.json_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "json"):
        sys.stdout.write("BENCHMARKING JSON ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_concurrency_script(self.json_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'json'))
        results = self.__parse_test('json')
        self.benchmarker.report_results(framework=self, test="json", results=results['results'])

        print "Complete"
    except AttributeError:
      pass

    # DB
    try:
      if self.db_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "db"):
        sys.stdout.write("BENCHMARKING DB ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_concurrency_script(self.db_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'db'))
        results = self.__parse_test('db')
        self.benchmarker.report_results(framework=self, test="db", results=results['results'])

        print "Complete"
    except AttributeError:
      pass

    # Query
    try:
      if self.query_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "query"):
        sys.stdout.write("BENCHMARKING Query ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_query_script(self.query_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'query'))
        results = self.__parse_test('query')
        self.benchmarker.report_results(framework=self, test="query", results=results['results'])
        print "Complete"
    except AttributeError:
      pass

    # fortune
    try:
      if self.fortune_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "fortune"):
        sys.stdout.write("BENCHMARKING Fortune ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_concurrency_script(self.fortune_url, self.port, self.accept_html)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'fortune'))
        results = self.__parse_test('fortune')
        self.benchmarker.report_results(framework=self, test="fortune", results=results['results'])
        print "Complete"
    except AttributeError:
      pass

    # update
    try:
      if self.update_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "update"):
        sys.stdout.write("BENCHMARKING Update ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_query_script(self.update_url, self.port, self.accept_json)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'update'))
        results = self.__parse_test('update')
        self.benchmarker.report_results(framework=self, test="update", results=results['results'])
        print "Complete"
    except AttributeError:
      pass

    # plaintext
    try:
      if self.plaintext_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "plaintext"):
        sys.stdout.write("BENCHMARKING Plaintext ... ") 
        sys.stdout.flush()
        remote_script = self.__generate_concurrency_script(self.plaintext_url, self.port, self.accept_plaintext, wrk_command="wrk-pipeline", intervals=[256,1024,4096,16384], pipeline="--pipeline 16")
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'plaintext'))
        results = self.__parse_test('plaintext')
        self.benchmarker.report_results(framework=self, test="plaintext", results=results['results'])
        print "Complete"
    except AttributeError:
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
      results['results'] = []
      
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
  def __run_benchmark(self, script, output_file):
    with open(output_file, 'w') as raw_file:
      p = subprocess.Popen(self.benchmarker.ssh_string.split(" "), stdin=subprocess.PIPE, stdout=raw_file, stderr=raw_file)
      p.communicate(script)
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

  ##########################################################################################
  # Constructor
  ##########################################################################################  
  def __init__(self, name, directory, benchmarker, args):
    self.name = name
    self.directory = directory
    self.benchmarker = benchmarker
    self.__dict__.update(args)

    # ensure diretory has __init__.py file so that we can use it as a Python package
    if not os.path.exists(os.path.join(directory, "__init__.py")):
      open(os.path.join(directory, "__init__.py"), 'w').close()

    self.setup_module = setup_module = importlib.import_module(directory + '.' + self.setup_file)
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
