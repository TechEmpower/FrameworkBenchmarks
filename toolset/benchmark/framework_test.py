from benchmark.fortune_html_parser import FortuneHTMLParser
from setup.linux import setup_util
from test_runner import TestRunner

import importlib
import os
import subprocess
import time
import re
import pprint
import sys
import traceback
import json
import logging
log = logging.getLogger('framework_test')
import inspect

from utils import WrapLogger
from utils import Header

class FrameworkTest:
  """
  Represents a framework test, including all types (JSON, plaintext, DB, etc)
  defined in that test. Used by Benchmarker to start, verify, benchmark, and 
  stop tests. Calls into the test's setup.py as needed. 

  Note: Any method in this class called from Benchmarker#__run_test is run 
        inside a thread
  Note: Many methods have a parameter 'logger' passed in from Benchmarker. 
        This uses python's logging module to support writing output to both a 
        file and stdout concurrently. If you wish to print something to stdout,
        regardless of the current global log level, use logger.info("Something"). 
        If you wish to respect the current global log level, use the logger 
        defined for this class e.g. log.info("Something else"). If you would 
        like to use this 'logger' with subprocess, see class WrapLogger
  """

  headers_template = "-H 'Host: localhost' -H '{accept}' -H 'Connection: keep-alive'"
  headers_full_template = "-H 'Host: localhost' -H '{accept}' -H 'Accept-Language: en-US,en;q=0.5' -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20130501 Firefox/30.0 AppleWebKit/600.00 Chrome/30.0.0000.0 Trident/10.0 Safari/600.00' -H 'Cookie: uid=12345678901234567890; __utma=1.1234567890.1234567890.1234567890.1234567890.12; wd=2560x1600' -H 'Connection: keep-alive'"
 
  accept_json = "Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"
  accept_html = "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  accept_plaintext = "Accept: text/plain,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7"

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
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Concurrency: $c for {name}"
      echo " {wrk} {headers} -d {duration} -c $c --timeout $c -t $(($c>{max_threads}?{max_threads}:$c)) \"http://{server_host}:{port}{url}\" -s ~/pipeline.lua -- {pipeline}"
      echo "---------------------------------------------------------"
      echo ""
      {wrk} {headers} -d {duration} -c $c --timeout $c -t "$(($c>{max_threads}?{max_threads}:$c))" http://{server_host}:{port}{url} -s ~/pipeline.lua -- {pipeline}
      sleep 2
    done
  """

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
    for c in {interval}
    do
      echo ""
      echo "---------------------------------------------------------"
      echo " Queries: $c for {name}"
      echo " wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} \"http://{server_host}:{port}{url}$c\""
      echo "---------------------------------------------------------"
      echo ""
      wrk {headers} -d {duration} -c {max_concurrency} --timeout {max_concurrency} -t {max_threads} "http://{server_host}:{port}{url}$c"
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

  ############################################################
  # Test Variables
  ############################################################
  JSON = "json"
  DB = "db"
  QUERY = "query"
  FORTUNE = "fortune"
  UPDATE = "update"
  PLAINTEXT = "plaintext"

  ##########################################################################################
  # Public Methods
  ##########################################################################################


  ############################################################
  # Validates the jsonString is a JSON object with a 'message'
  # key with the value "hello, world!" (case-insensitive).
  ############################################################
  def validateJson(self, jsonString, logger=log):
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).items()}

      if  obj["message"].lower() == "hello, world!":
        return True
    except:
      logger.debug("Expected: %s", "{'message':'hello, world!'}")
      logger.debug("Got: '%s'", jsonString)
      pass
    return False

  ############################################################
  # Validates the jsonString is a JSON object that has an "id"
  # and a "randomNumber" key, and that both keys map to 
  # integers.
  ############################################################
  def validateDb(self, jsonString, logger=log):
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).items()}

      # We are allowing the single-object array for the DB 
      # test for now, but will likely remove this later.
      if type(obj) == list:
        obj = obj[0]

      # This will error out of the value could not parsed to a
      # float (this will work with ints, but it will turn them
      # into their float equivalent; i.e. "123" => 123.0)
      if (type(float(obj["id"])) == float and 
          type(float(obj["randomnumber"])) == float):
        return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  def validateDbStrict(self, jsonString, logger=log):
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).items()}

      # This will error out of the value could not parsed to a
      # float (this will work with ints, but it will turn them
      # into their float equivalent; i.e. "123" => 123.0)
      if (type(float(obj["id"])) == float and 
          type(float(obj["randomnumber"])) == float):
        return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False


  ############################################################
  # Validates the jsonString is an array with a length of
  # 2, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQuery(self, jsonString, logger=log):
    try:
      arr = [{k.lower(): v for k,v in d.items()} for d in json.loads(jsonString)]

      if (type(float(arr[0]["id"])) == float and 
          type(float(arr[0]["randomnumber"])) == float and 
          type(float(arr[1]["id"])) == float and 
          type(float(arr[1]["randomnumber"])) == float):
        return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  ############################################################
  # Validates the jsonString is an array with a length of
  # 1, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQueryOneOrLess(self, jsonString, logger=log):
    try:
      arr = {k.lower(): v for k,v in json.loads(jsonString).items()}

      if len(arr) != 1:
        return False

      for obj in arr:
        if (type(float(obj["id"])) != float or
            type(float(obj["randomnumber"])) != float or
            type(float(obj["id"])) != float or
            type(float(obj["randomnumber"])) != float):
          return False
      # By here, it's passed validation
      return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  ############################################################
  # Validates the jsonString is an array with a length of
  # 500, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQueryFiveHundredOrMore(self, jsonString, logger=log):
    try:
      arr = {k.lower(): v for k,v in json.loads(jsonString).items()}

      if len(arr) != 500:
        return False

      for obj in arr:
        if (type(float(obj["id"])) != float or
            type(float(obj["randomnumber"])) != float or
            type(float(obj["id"])) != float or
            type(float(obj["randomnumber"])) != float):
          return False
      # By here, it's passed validation
      return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  ############################################################
  # Parses the given HTML string and asks a FortuneHTMLParser
  # whether the parsed string is a valid fortune return.
  ############################################################
  def validateFortune(self, htmlString, logger=log):
    try:
      parser = FortuneHTMLParser()
      parser.feed(htmlString)

      return parser.isValidFortune()
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", htmlString)
      pass
    return False

  ############################################################
  # Validates the jsonString is an array with a length of
  # 2, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateUpdate(self, jsonString, logger=log):
    try:
      arr = [{k.lower(): v for k,v in d.items()} for d in json.loads(jsonString)]

      if (type(float(arr[0]["id"])) == float and 
          type(float(arr[0]["randomnumber"])) == float and 
          type(float(arr[1]["id"])) == float and 
          type(float(arr[1]["randomnumber"])) == float):
        return True
    except:
      # logger.debug("Expected: %s", "")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  ############################################################
  #
  ############################################################
  def validatePlaintext(self, jsonString, logger=log):
    try:
      return jsonString.lower().strip() == "hello, world!"
    except:
      logger.debug("Expected: %s", "hello, world!")
      logger.debug("Got: %s", jsonString)
      pass
    return False

  ############################################################
  # start(benchmarker)
  # Start the test using it's setup file
  ############################################################
  def start(self, logger=log):
    log.info("start")

    # Load profile for this installation
    profile="%s/bash_profile.sh" % self.directory
    if not os.path.exists(profile):
      logger.warning("Framework %s does not have a bash_profile", self.name)
      profile="$FWROOT/config/benchmark_profile"
    
    set_iroot="export IROOT=%s" % self.install_root
    setup_util.replace_environ(config=profile, command=set_iroot)
    
    # Determine if setup.py contains a subclass of TestRunner
    self.runner = None
    for name, obj in inspect.getmembers(self.setup_module, inspect.isclass):
      try:
        is_subclass = TestRunner.is_parent_of(obj)
      except Exception as e: 
        logger.critical("%s: %s", self.setup_module.__file__, e)
        return 1

      if is_subclass:
        logger.debug("Framework %s is using the new setup.py format" % self.name)
        self.runner = obj(self, self.setup_module, logger)
        return self.runner.start()

    # If not, call the start function directly
    logger.warning("Framework %s is using the old setup.py format" % self.name)
    (out, err) = WrapLogger(logger, logging.INFO), WrapLogger(logger, logging.ERROR)
    return self.setup_module.start(self.benchmarker, out, err)
  ############################################################
  # End start
  ############################################################

  ############################################################
  # stop(benchmarker)
  # Stops the test using it's setup file
  ############################################################
  def stop(self, logger=log):
    log.info("stop")

    # Are we using a TestRunner
    if self.runner: 
      return self.runner.stop()  

    (out, err) = WrapLogger(logger, logging.INFO), WrapLogger(logger, logging.ERROR)
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
  def verify_urls(self, logger=log):

    # JSON
    if self.runTests[self.JSON]:
      logger.info(Header("VERIFYING JSON (%s)" % self.json_url))
      url = self.benchmarker.generate_url(self.json_url, self.port)
      output = self.__curl_url(url, self.JSON, logger)
      logger.info("VALIDATING JSON ... ")
      if self.validateJson(output, logger):
        self.json_url_passed = True
        logger.info("PASS")
      else:
        self.json_url_passed = False
        logger.info("FAIL")

    # DB
    if self.runTests[self.DB]:
      logger.info(Header("VERIFYING DB (%s)" % self.db_url))
      url = self.benchmarker.generate_url(self.db_url, self.port)
      output = self.__curl_url(url, self.DB, logger)
      if self.validateDb(output, logger):
        self.db_url_passed = True
      else:
        self.db_url_passed = False
      if self.validateDbStrict(output, logger):
        self.db_url_warn = False
      else:
        self.db_url_warn = True

      logger.info("VALIDATING DB ... ")
      if self.db_url_passed:
        if self.db_url_warn:
          logger.info("PASS (with warnings)")
        else:
          logger.info("PASS")
      else:
        logger.info("FAIL")

    # Query
    if self.runTests[self.QUERY]:
      logger.info(Header("VERIFYING QUERY (%s)" % self.query_url+"2"))
      url = self.benchmarker.generate_url(self.query_url + "2", self.port)
      output = self.__curl_url(url, self.QUERY, logger)
      if self.validateQuery(output, logger):
        self.query_url_passed = True
        logger.info(self.query_url + "2 - PASS")
      else:
        self.query_url_passed = False
        logger.info(self.query_url + "2 - FAIL")
      logger.info("-----------------------------------------------------")
      
      self.query_url_warn = False
      url2 = self.benchmarker.generate_url(self.query_url + "0", self.port)
      output2 = self.__curl_url(url2, self.QUERY, logger)
      if not self.validateQueryOneOrLess(output2, logger):
        self.query_url_warn = True
        logger.info(self.query_url + "0 - WARNING")
      else:
        logger.info(self.query_url + "0 - PASS")
      logger.info("-----------------------------------------------------")

      url3 = self.benchmarker.generate_url(self.query_url + "foo", self.port)
      output3 = self.__curl_url(url3, self.QUERY, logger)
      if not self.validateQueryOneOrLess(output3, logger):
        self.query_url_warn = True
        logger.info(self.query_url + "foo - WARNING")
      else:
        logger.info(self.query_url + "foo - PASS")
      logger.info("-----------------------------------------------------")  

      url4 = self.benchmarker.generate_url(self.query_url + "501", self.port)
      output4 = self.__curl_url(url4, self.QUERY, logger)
      if not self.validateQueryFiveHundredOrMore(output4, logger):
        self.query_url_warn = True
        logger.info(self.query_url + "501 - WARNING")
      else:
        logger.info(self.query_url + "501 - PASS")
      logger.info("-----------------------------------------------------")

      logger.info("VALIDATING QUERY ... ")
      if self.query_url_passed and self.query_url_warn:
        logger.info("PASS (with warnings)")
      elif self.query_url_passed:
        logger.info("PASS")
      else:
        logger.info("FAIL")

    # Fortune
    if self.runTests[self.FORTUNE]:
      logger.info(Header("VERIFYING FORTUNE (%s)" % self.fortune_url))
      url = self.benchmarker.generate_url(self.fortune_url, self.port)
      output = self.__curl_url(url, self.FORTUNE, logger)
      logger.info("VALIDATING FORTUNE ... ")
      if self.validateFortune(output, logger):
        self.fortune_url_passed = True
        logger.info("PASS")
      else:
        self.fortune_url_passed = False
        logger.info("FAIL")

    # Update
    if self.runTests[self.UPDATE]:
      logger.info(Header("VERIFYING UPDATE (%s)" % self.update_url))
      url = self.benchmarker.generate_url(self.update_url + "2", self.port)
      output = self.__curl_url(url, self.UPDATE, logger)
      logger.info("VALIDATING UPDATE ... ")
      if self.validateUpdate(output, logger):
        self.update_url_passed = True
        logger.info("PASS")
      else:
        self.update_url_passed = False
        logger.info("FAIL")

    # plaintext
    if self.runTests[self.PLAINTEXT]:
      logger.info(Header("VERIFYING PLAINTEXT (%s)" % self.plaintext_url))
      url = self.benchmarker.generate_url(self.plaintext_url, self.port)
      output = self.__curl_url(url, self.PLAINTEXT, logger)
      logger.info("VALIDATING PLAINTEXT ... ")
      if self.validatePlaintext(output, logger):
        self.plaintext_url_passed = True
        logger.info("PASS")
      else:
        self.plaintext_url_passed = False
        logger.info("FAIL")

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
      if type == self.JSON and self.json_url is not None:
        return True
      if type == self.DB and self.db_url is not None:
        return True
      if type == self.QUERY and self.query_url is not None:
        return True
      if type == self.FORTUNE and self.fortune_url is not None:
        return True
      if type == self.UPDATE and self.update_url is not None:
        return True
      if type == self.PLAINTEXT and self.plaintext_url is not None:
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
  def benchmark(self, logger=log):
    
    # JSON
    if self.runTests[self.JSON]:
      try:
        logger.info("BENCHMARKING JSON ... ") 
        results = None
        output_file = self.benchmarker.output_file(self.name, self.JSON)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.json_url_passed:
          remote_script = self.__generate_concurrency_script(self.json_url, self.port, self.accept_json)
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.JSON)
        self.benchmarker.report_results(framework=self, test=self.JSON, results=results['results'])
        logger.info("Complete")
      except AttributeError:
        pass

    # DB
    if self.runTests[self.DB]:
      try:
        logger.info("BENCHMARKING DB ... ") 
        results = None
        output_file = self.benchmarker.output_file(self.name, self.DB)
        warning_file = self.benchmarker.warning_file(self.name, self.DB)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.db_url_warn:
          with open(warning_file, 'w'):
            pass
        if self.db_url_passed:
          remote_script = self.__generate_concurrency_script(self.db_url, self.port, self.accept_json)
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.DB)
        self.benchmarker.report_results(framework=self, test=self.DB, results=results['results'])
        logger.info("Complete")
      except AttributeError:
        pass

    # Query
    if self.runTests[self.QUERY]:
      try:
        logger.info("BENCHMARKING Query ...")
        results = None
        output_file = self.benchmarker.output_file(self.name, self.QUERY)
        warning_file = self.benchmarker.warning_file(self.name, self.QUERY)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.query_url_warn:
          with open(warning_file, 'w'):
            pass
        if self.query_url_passed:
          remote_script = self.__generate_query_script(self.query_url, self.port, self.accept_json)
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.QUERY)
        self.benchmarker.report_results(framework=self, test=self.QUERY, results=results['results'])
        logger.info("Complete")
      except AttributeError:
        pass

    # fortune
    if self.runTests[self.FORTUNE]:
      try:
        logger.info("BENCHMARKING Fortune ... ") 
        results = None
        output_file = self.benchmarker.output_file(self.name, self.FORTUNE)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.fortune_url_passed:
          remote_script = self.__generate_concurrency_script(self.fortune_url, self.port, self.accept_html)
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.FORTUNE)
        self.benchmarker.report_results(framework=self, test=self.FORTUNE, results=results['results'])
        logger.info("Complete")
      except AttributeError:
        pass

    # update
    if self.runTests[self.UPDATE]:
      try:
        logger.info("BENCHMARKING Update ... ") 
        results = None
        output_file = self.benchmarker.output_file(self.name, self.UPDATE)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.update_url_passed:
          remote_script = self.__generate_query_script(self.update_url, self.port, self.accept_json)
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.UPDATE)
        self.benchmarker.report_results(framework=self, test=self.UPDATE, results=results['results'])
        logger.info( "Complete" )
      except AttributeError:
        pass

    # plaintext
    if self.runTests[self.PLAINTEXT]:
      try:
        logger.info("BENCHMARKING Plaintext ... ")
        results = None
        output_file = self.benchmarker.output_file(self.name, self.PLAINTEXT)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.plaintext_url_passed:
          remote_script = self.__generate_concurrency_script(self.plaintext_url, self.port, self.accept_plaintext, wrk_command="wrk", intervals=[256,1024,4096,16384], pipeline="16")
          self.__run_benchmark(remote_script, output_file, logger)
        results = self.__parse_test(self.PLAINTEXT)
        self.benchmarker.report_results(framework=self, test=self.PLAINTEXT, results=results['results'])
        logger.info( "Complete" )
      except AttributeError:
        logger.exception("Error Running Benchmark for %s", self.name)
        pass

  ############################################################
  # End benchmark
  ############################################################
  
  ############################################################
  # parse_all
  # Method meant to be run for a given timestamp
  ############################################################
  def parse_all(self):
    log.info("parse_all")
    # JSON
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.JSON)):
      results = self.__parse_test(self.JSON)
      self.benchmarker.report_results(framework=self, test=self.JSON, results=results['results'])
    
    # DB
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.DB)):
      results = self.__parse_test(self.DB)
      self.benchmarker.report_results(framework=self, test=self.DB, results=results['results'])
    
    # Query
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.QUERY)):
      results = self.__parse_test(self.QUERY)
      self.benchmarker.report_results(framework=self, test=self.QUERY, results=results['results'])

    # Fortune
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.FORTUNE)):
      results = self.__parse_test(self.FORTUNE)
      self.benchmarker.report_results(framework=self, test=self.FORTUNE, results=results['results'])

    # Update
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.UPDATE)):
      results = self.__parse_test(self.UPDATE)
      self.benchmarker.report_results(framework=self, test=self.UPDATE, results=results['results'])

    # Plaintext
    if os.path.exists(self.benchmarker.get_output_file(self.name, self.PLAINTEXT)):
      results = self.__parse_test(self.PLAINTEXT)
      self.benchmarker.report_results(framework=self, test=self.PLAINTEXT, results=results['results'])
  ############################################################
  # End parse_all
  ############################################################

  ############################################################
  # __parse_test(test_type)
  ############################################################
  def __parse_test(self, test_type):
    log.info("__parse_test") 
    try:
      results = dict()
      results['results'] = []
      
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
  def __run_benchmark(self, script, output_file, logger):
    err = WrapLogger(logger, logging.ERROR)
    with open(output_file, 'w') as raw_file:
      p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE, stdout=raw_file, stderr=err)
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

  ############################################################
  # __curl_url
  # Dump HTTP response and headers. Throw exception if there
  # is an HTTP error.
  ############################################################
  def __curl_url(self, url, testType, logger=log):
    # Send output to our benchmark's logger for archival to file,
    # but only show err on stdout by default
    (out, err) = WrapLogger(logger, logging.DEBUG), WrapLogger(logger, logging.ERROR)
    output = None
    try:
      # Use -m 15 to make curl stop trying after 15sec.
      # Use -i to output response with headers.
      # Don't use -f so that the HTTP response code is ignored.
      # Use --stderr - to redirect stderr to stdout so we get
      # error output for sure in stdout.
      # Use -sS to hide progress bar, but show errors.
      subprocess.check_call(["curl", "-m", "15", "-i", "-sS", url], stderr=err, stdout=out)

      # We need to get the respond body from the curl and return it.
      p = subprocess.Popen(["curl", "-m", "15", "-s", url], stdout=subprocess.PIPE)
      output = p.communicate()
    except:
      pass

    if output:
      # We have the response body - return it
      return output[0]
  ##############################################################
  # End __curl_url
  ##############################################################

  def requires_database(self):
      """Returns True/False if this test requires a database"""
      return (self.runTests[self.FORTUNE] or 
              self.runTests[self.DB] or 
              self.runTests[self.QUERY] or
              self.runTests[self.UPDATE])

  ##########################################################################################
  # Constructor
  ##########################################################################################  
  def __init__(self, name, directory, benchmarker, runTests, args):
    self.name = name
    self.directory = directory
    self.benchmarker = benchmarker
    self.runTests = runTests
    self.fwroot = benchmarker.fwroot
        
    self.install_root="%s/%s" % (self.fwroot, "installs")
    if benchmarker.install_strategy is 'pertest':
      self.install_root="%s/pertest/%s" % (self.install_root, name)

    self.__dict__.update(args)

    # ensure directory has __init__.py file so that we can use it as a Python package
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
      
      runTests = dict()

      runTests["json"] = (benchmarker.type == "all" or benchmarker.type == "json") and value.get("json_url", False)
      runTests["db"] = (benchmarker.type == "all" or benchmarker.type == "db") and value.get("db_url", False)
      runTests["query"] = (benchmarker.type == "all" or benchmarker.type == "query") and value.get("query_url", False)
      runTests["fortune"] = (benchmarker.type == "all" or benchmarker.type == "fortune") and value.get("fortune_url", False)
      runTests["update"] = (benchmarker.type == "all" or benchmarker.type == "update") and value.get("update_url", False)
      runTests["plaintext"] = (benchmarker.type == "all" or benchmarker.type == "plaintext") and value.get("plaintext_url", False)

      # if the test uses the 'defualt' keywork, then we don't 
      # append anything to it's name. All configs should only have 1 default
      if key != 'default':
        # we need to use the key in the test_name
        test_name = test_name + "-" + key

      tests.append(FrameworkTest(test_name, directory, benchmarker, runTests, value))

  return tests
##############################################################
# End parse_config
##############################################################
