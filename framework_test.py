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
  concurrency_template = """
    mysqladmin flush-hosts -uroot -psecret
    
    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}"
    echo "---------------------------------------------------------"
    echo ""
    wrk -r 1000 -c 8 -t 8 http://{server_host}:{port}{url}
    sleep 5
    wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Concurrency: $c for {name}"
      echo " wrk -n {runs} -c $c -t $(($c>{max_threads}?{max_threads}:$c)) http://{server_host}:{port}{url}"
      echo "---------------------------------------------------------"
      echo ""
      wrk -r {runs} -c "$c" -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url}
    done
  """

  query_template = """
    mysqladmin flush-hosts -uroot -psecret

    echo ""
    echo "---------------------------------------------------------"
    echo " Running Warmup {name}"
    echo " wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}2"
    echo "---------------------------------------------------------"
    echo ""
    wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}2
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Queries: $c for {name}"
      echo " wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}$c"
      echo "---------------------------------------------------------"
      echo ""
      wrk -r {runs} -c {max_concurrency} -t {max_threads} http://{server_host}:{port}{url}"$c"
    done
  """

  # The sort value is the order in which we represent all the tests. (Mainly helpful for our charts to give the underlying data)
  # a consistent ordering even when we add or remove tests. Each test should give a sort value in it's benchmark_config file.
  sort = 1000

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
  ############################################################
  # End verify_urls
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
        remote_script = self.__generate_concurrency_script(self.json_url, self.port)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'json'))
        results = self.__parse_test('json')
        self.benchmarker.report_results(framework=self, test="json", requests=results['requests'], latency=results['latency'],
          results=results['results'], total_time=results['total_time'])

        print "Complete"
    except AttributeError:
      pass

    # DB
    try:
      if self.db_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "db"):
        sys.stdout.write("BENCHMARKING DB ... ") 
        remote_script = self.__generate_concurrency_script(self.db_url, self.port)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'db'))
        results = self.__parse_test('db')
        self.benchmarker.report_results(framework=self, test="db", requests=results['requests'], latency=results['latency'],
          results=results['results'], total_time=results['total_time'])

        print "Complete"
    except AttributeError:
      pass

    # Query
    try:
      if self.query_url_passed and (self.benchmarker.type == "all" or self.benchmarker.type == "query"):
        sys.stdout.write("BENCHMARKING Query ... ") 
        remote_script = self.__generate_query_script(self.query_url, self.port)
        self.__run_benchmark(remote_script, self.benchmarker.output_file(self.name, 'query'))
        results = self.__parse_test('query')
        self.benchmarker.report_results(framework=self, test="query", requests=results['requests'], latency=results['latency'],
          results=results['results'], total_time=results['total_time'])
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
      self.benchmarker.report_results(framework=self, test="json", requests=results['requests'], latency=results['latency'],
        results=results['results'], total_time=results['total_time'])
    
    # DB
    if os.path.exists(self.benchmarker.output_file(self.name, 'db')):
      results = self.__parse_test('db')
      self.benchmarker.report_results(framework=self, test="db", requests=results['requests'], latency=results['latency'],
        results=results['results'], total_time=results['total_time'])
    
    # Query
    if os.path.exists(self.benchmarker.output_file(self.name, 'query')):
      results = self.__parse_test('query')
      self.benchmarker.report_results(framework=self, test="query", requests=results['requests'], latency=results['latency'],
        results=results['results'], total_time=results['total_time'])
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
      results['total_time'] = 0
      results['latency'] = dict()
      results['latency']['avg'] = 0
      results['latency']['stdev'] = 0
      results['latency']['max'] = 0
      results['latency']['stdevPercent'] = 0
      results['requests'] = dict()
      results['requests']['avg'] = 0
      results['requests']['stdev'] = 0
      results['requests']['max'] = 0
      results['requests']['stdevPercent'] = 0
      with open(self.benchmarker.output_file(self.name, test_type)) as raw_data:
        found_warmup = False
        for line in raw_data:
          # wrk outputs a line with the "Requests/sec:" number for each run
          if "Requests/sec:" in line:
            # Every raw data file first has a warmup run, so we need to pass over that before we begin parsing
            if not found_warmup:
              found_warmup = True
              continue

            m = re.search("Requests/sec:\s+([0-9]+)", line)
            results['results'].append(m.group(1))

          if found_warmup:
            # search for weighttp data such as succeeded and failed.
            if "Latency" in line:
              m = re.findall("([0-9]+\.*[0-9]*[us|ms|s|m|%]+)", line)
              if len(m) == 4:
                results['latency']['avg'] = m[0]
                results['latency']['stdev'] = m[1]
                results['latency']['max'] = m[2]
                results['latency']['stdevPercent'] = m[3]
            
            if "Req/Sec" in line:
              m = re.findall("([0-9]+\.*[0-9]*[k|%]*)", line)
              if len(m) == 4:
                results['requests']['avg'] = m[0]
                results['requests']['stdev'] = m[1]
                results['requests']['max'] = m[2]
                results['requests']['stdevPercent'] = m[3]
              
            if "requests in" in line:
              m = re.search("requests in ([0-9]+\.*[0-9]*[ms|s|m|h]+)", line)
              if m != None: 
                # parse out the raw time, which may be in minutes or seconds
                raw_time = m.group(1)
                if "ms" in raw_time:
                  results['total_time'] += float(raw_time[:len(raw_time)-2]) / 1000.0
                elif "s" in raw_time:
                  results['total_time'] += float(raw_time[:len(raw_time)-1])
                elif "m" in raw_time:
                  results['total_time'] += float(raw_time[:len(raw_time)-1]) * 60.0
                elif "h" in raw_time:
                  results['total_time'] += float(raw_time[:len(raw_time)-1]) * 3600.0

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
  def __generate_concurrency_script(self, url, port):
    return self.concurrency_template.format(max_concurrency=self.benchmarker.max_concurrency, 
      max_threads=self.benchmarker.max_threads, name=self.name, runs=self.benchmarker.number_of_runs, 
      interval=" ".join("{}".format(item) for item in self.benchmarker.concurrency_levels), 
      server_host=self.benchmarker.server_host, port=port, url=url)
  ############################################################
  # End __generate_concurrency_script
  ############################################################

  ############################################################
  # __generate_query_script(url, port)
  # Generates the string containing the bash script that will
  # be run on the client to benchmark a single test. This
  # specifically works for the variable query tests (Query)
  ############################################################
  def __generate_query_script(self, url, port):
    return self.query_template.format(max_concurrency=self.benchmarker.max_concurrency, 
      max_threads=self.benchmarker.max_threads, name=self.name, runs=self.benchmarker.number_of_runs, 
      interval=" ".join("{}".format(item) for item in self.benchmarker.query_intervals), 
      server_host=self.benchmarker.server_host, port=port, url=url)
  ############################################################
  # End __generate_query_script
  ############################################################

  ##########################################################################################
  # Constructor
  ##########################################################################################  
  def __init__(self, name, directory, benchmarker, args):
    self.name = name
    self.directory = directory
    self.benchmarker = benchmarker
    self.__dict__.update(args)

    # ensure diretory has __init__.py file so that we can use it as a pythong package
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
