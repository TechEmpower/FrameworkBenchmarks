from benchmark.fortune_html_parser import FortuneHTMLParser
from setup.linux import setup_util

import importlib
import os
import subprocess
import time
import re
import pprint
import sys
import traceback
import json
import textwrap
import logging
import csv
import shlex
import math

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

    for c in {interval}
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

    for c in {interval}
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
  def validateJson(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).iteritems()}
      if "message" not in obj:
        err_str += "Expected key 'message' to be in JSON string "
      if  obj["message"].lower() != "hello, world!":
        err_str += "Message was '{message}', should have been 'Hello, World!' ".format(message=obj["message"])
    except:
      err_str += "Got exception when trying to validate the JSON test: {exception}".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  # Validates the jsonString is a JSON object that has an "id"
  # and a "randomNumber" key, and that both keys map to 
  # integers.
  ############################################################
  def validateDb(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).iteritems()}

      # We are allowing the single-object array for the DB 
      # test for now, but will likely remove this later.
      if type(obj) == list:
        obj = obj[0]

      if "id" not in obj or "randomnumber" not in obj:
        err_str += "Expected keys id and randomNumber to be in JSON string. "
        return (False, err_str)

      # This will error out of the value could not parsed to a
      # float (this will work with ints, but it will turn them
      # into their float equivalent; i.e. "123" => 123.0)
      id_ret_val = True
      try:
        if not isinstance(float(obj["id"]), float):
          id_ret_val=False
      except:
        id_ret_val=False
      if not id_ret_val:
        err_str += "Expected id to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
      random_num_ret_val = True
      try:
        if not isinstance(float(obj["randomnumber"]), float):
          random_num_ret_val=False
      except:
        random_num_ret_val=False
      if not random_num_ret_val:
        err_str += "Expected id to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
    except:
      err_str += "Got exception when trying to validate the db test: {exception}".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  def validateDbStrict(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      obj = {k.lower(): v for k,v in json.loads(jsonString).iteritems()}

      # This will error out of the value could not parsed to a
      # float (this will work with ints, but it will turn them
      # into their float equivalent; i.e. "123" => 123.0)
      id_ret_val = True
      try:
        if not isinstance(float(obj["id"]), float):
          id_ret_val=False
      except:
        id_ret_val=False
      if not id_ret_val:
        err_str += "Expected id to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
      random_num_ret_val = True
      try:
        if not isinstance(float(obj["randomnumber"]), float):
          random_num_ret_val=False
      except:
        random_num_ret_val=False
      if not random_num_ret_val:
        err_str += "Expected id to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
      return id_ret_val and random_num_ret_val
    except:
      err_str += "Got exception when trying to validate the db test: {exception}".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)


  ############################################################
  # Validates the jsonString is an array with a length of
  # 2, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQuery(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      arr = [{k.lower(): v for k,v in d.iteritems()} for d in json.loads(jsonString)]
      if len(arr) != 2:
        err_str += "Expected array of length 2. Got length {length}. ".format(length=len(arr))
      for obj in arr:
        id_ret_val = True
        random_num_ret_val = True
        if "id" not in obj or "randomnumber" not in obj:
          err_str += "Expected keys id and randomNumber to be in JSON string. "
          break
        try:
          if not isinstance(float(obj["id"]), float):
            id_ret_val=False
        except:
          id_ret_val=False
        if not id_ret_val:
          err_str += "Expected id to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
        try:
          if not isinstance(float(obj["randomnumber"]), float):
            random_num_ret_val=False
        except:
          random_num_ret_val=False
        if not random_num_ret_val:
          err_str += "Expected randomNumber to be type int or float, got '{rand}' ".format(rand=obj["randomnumber"])
    except:
      err_str += "Got exception when trying to validate the query test: {exception}".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  # Validates the jsonString is an array with a length of
  # 1, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQueryOneOrLess(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      json_load = json.loads(jsonString)
      if not isinstance(json_load, list):
        err_str += "Expected JSON array, got {typeObj}. ".format(typeObj=type(json_load))
        return (False, err_str)
      if len(json_load) != 1:
        err_str += "Expected array of length 1. Got length {length}. ".format(length=len(json_load))

      arr = {k.lower(): v for k,v in json_string[0].iteritems()}

      for obj in arr:
        id_ret_val = True
        random_num_ret_val = True
        if "id" not in obj or "randomnumber" not in obj:
          err_str += "Expected keys id and randomNumber to be in JSON string. "
          continue
        try:
          if not isinstance(float(obj["id"]), float):
            id_ret_val=False
        except:
          id_ret_val=False
        if not id_ret_val:
          err_str += "Expected id to be type int or float, got '{rand}'. ".format(rand=obj["randomnumber"])
        try:
          if not isinstance(float(obj["randomnumber"]), float):
            random_num_ret_val=False
        except:
          random_num_ret_val=False
        if not random_num_ret_val:
          err_str += "Expected randomNumber to be type int or float, got '{rand}'. ".format(rand=obj["randomnumber"])
          ret_val = False
      return ret_val
    except:
      err_str += "Got exception when trying to validate the query test: {exception} ".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  # Validates the jsonString is an array with a length of
  # 500, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateQueryFiveHundredOrMore(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      arr = [{k.lower(): v for k,v in d.iteritems()} for d in json.loads(jsonString)]

      if len(arr) != 500:
        err_str += "Expected array of length 500. Got length {length}. ".format(length=len(arr))
        return False

      for obj in arr:
        id_ret_val = True
        random_num_ret_val = True
        if "id" not in obj or "randomnumber" not in obj:
          err_str += "Expected keys id and randomNumber to be in JSON string. "
          break
        try:
          if not isinstance(float(obj["id"]), float):
            id_ret_val=False
        except:
          id_ret_val=False
        if not id_ret_val:
          err_str += "Expected id to be type int or float, got '{rand}'. ".format(rand=obj["randomnumber"])
        try:
          if not isinstance(float(obj["randomnumber"]), float):
            random_num_ret_val=False
        except:
          random_num_ret_val=False
        if not random_num_ret_val:
          err_str += "Expected randomNumber to be type int or float, got '{rand}'. ".format(rand=obj["randomnumber"])
    except:
      err_str += "Got exception when trying to validate the query test: {exception} ".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  # Parses the given HTML string and asks a FortuneHTMLParser
  # whether the parsed string is a valid fortune return.
  ############################################################
  def validateFortune(self, htmlString, out, err):
    err_str = ""
    if htmlString is None or len(htmlString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      parser = FortuneHTMLParser()
      parser.feed(htmlString)

      return parser.isValidFortune(out)
    except:
      print "Got exception when trying to validate the fortune test: {exception} ".format(exception=traceback.print_exc())
    return False

  ############################################################
  # Validates the jsonString is an array with a length of
  # 2, that each entry in the array is a JSON object, that
  # each object has an "id" and a "randomNumber" key, and that
  # both keys map to integers.
  ############################################################
  def validateUpdate(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      arr = [{k.lower(): v for k,v in d.iteritems()} for d in json.loads(jsonString)]
      print arr
      if len(arr) != 2:
        err_str += "Expected array of length 2. Got length {length}.\n".format(length=len(arr))
      for obj in arr:
        id_ret_val = True
        random_num_ret_val = True
        if "id" not in obj or "randomnumber" not in obj:
          err_str += "Expected keys id and randomNumber to be in JSON string.\n"
          return False
        try:
          if not isinstance(float(obj["id"]), float):
            id_ret_val=False
        except:
          id_ret_val=False
        if not id_ret_val:
          err_str += "Expected id to be type int or float, got '{rand}'.\n".format(rand=obj["randomnumber"])
        try:
          if not isinstance(float(obj["randomnumber"]), float):
            random_num_ret_val=False
        except:
          random_num_ret_val=False
        if not random_num_ret_val:
          err_str += "Expected randomNumber to be type int or float, got '{rand}'.\n".format(rand=obj["randomnumber"])
    except:
      err_str += "Got exception when trying to validate the update test: {exception}\n".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  #
  ############################################################
  def validatePlaintext(self, jsonString, out, err):
    err_str = ""
    if jsonString is None or len(jsonString) == 0:
      err_str += "Empty Response"
      return (False, err_str)
    try:
      if not jsonString.lower().strip() == "hello, world!":
        err_str += "Expected 'Hello, World!', got '{message}'.\n".format(message=jsonString.strip())
    except:
      err_str += "Got exception when trying to validate the plaintext test: {exception}\n".format(exception=traceback.print_exc())
    return (True, ) if len(err_str) == 0 else (False, err_str)

  ############################################################
  # start(benchmarker)
  # Start the test using it's setup file
  ############################################################
  def start(self, out, err):
    # Load profile for this installation
    profile="%s/bash_profile.sh" % self.directory
    if not os.path.exists(profile):
      logging.warning("Framework %s does not have a bash_profile" % self.name)
      profile="$FWROOT/config/benchmark_profile"
    
    test_rel_dir = setup_util.path_relative_to_root(self.directory)
    setup_util.replace_environ(config=profile, 
              command='export TROOT=$FWROOT%s && export IROOT=%s' %
              (test_rel_dir, self.install_root))

    return self.setup_module.start(self.benchmarker, out, err)
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
      logging.warning("Framework %s does not have a bash_profile" % self.name)
      profile="$FWROOT/config/benchmark_profile"
    
    test_rel_dir = setup_util.path_relative_to_root(self.directory)
    setup_util.replace_environ(config=profile, 
              command='export TROOT=$FWROOT%s && export IROOT=%s' %
              (test_rel_dir, self.install_root))

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
  # Returns True if all verifications succeeded
  ############################################################
  def verify_urls(self, out, err):
    result = True

    # JSON
    if self.runTests[self.JSON]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING JSON ({url})
        -----------------------------------------------------
        """.format(url = self.json_url)))
      out.flush()

      url = self.benchmarker.generate_url(self.json_url, self.port)
      output = self.__curl_url(url, self.JSON, out, err)
      out.write("VALIDATING JSON ... ")
      ret_tuple = self.validateJson(output, out, err)
      if ret_tuple[0]:
        self.json_url_passed = True
        out.write("PASS\n\n")
      else:
        self.json_url_passed = False
        out.write("\nFAIL" + ret_tuple[1] + "\n\n")
        result = False
      out.flush()

    # DB
    if self.runTests[self.DB]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING DB ({url})
        -----------------------------------------------------
        """.format(url = self.db_url)))
      out.flush()

      url = self.benchmarker.generate_url(self.db_url, self.port)
      output = self.__curl_url(url, self.DB, out, err)
      validate_ret_tuple = self.validateDb(output, out, err)
      validate_strict_ret_tuple = self.validateDbStrict(output, out, err)
      if validate_ret_tuple[0]:
        self.db_url_passed = True
      else:
        self.db_url_passed = False
      if validate_strict_ret_tuple:
        self.db_url_warn = False
      else:
        self.db_url_warn = True

      out.write("VALIDATING DB ... ")
      if self.db_url_passed:
        out.write("PASS")
        if self.db_url_warn:
          out.write(" (with warnings) " + validate_strict_ret_tuple[1])
        out.write("\n\n")
      else:
        out.write("\nFAIL" + validate_ret_tuple[1])
        result = False
      out.flush()

    # Query
    if self.runTests[self.QUERY]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING QUERY ({url})
        -----------------------------------------------------
        """.format(url=self.query_url+"2")))
      out.flush()

      url = self.benchmarker.generate_url(self.query_url + "2", self.port)
      output = self.__curl_url(url, self.QUERY, out, err)
      ret_tuple = self.validateQuery(output, out, err)
      if ret_tuple[0]:
        self.query_url_passed = True
        out.write(self.query_url + "2 - PASS\n\n")
      else:
        self.query_url_passed = False
        out.write(self.query_url + "2 - FAIL " + ret_tuple[1] + "\n\n")
      out.write("-----------------------------------------------------\n\n")
      out.flush()

      self.query_url_warn = False
      url2 = self.benchmarker.generate_url(self.query_url + "0", self.port)
      output2 = self.__curl_url(url2, self.QUERY, out, err)
      ret_tuple = self.validateQueryOneOrLess(output2, out, err)
      if not ret_tuple[0]:
        self.query_url_warn = True
        out.write(self.query_url + "0 - WARNING " + ret_tuple[1] + "\n\n")
      else:
        out.write(self.query_url + "0 - PASS\n\n")
      out.write("-----------------------------------------------------\n\n")
      out.flush()

      url3 = self.benchmarker.generate_url(self.query_url + "foo", self.port)
      output3 = self.__curl_url(url3, self.QUERY, out, err)
      ret_tuple = self.validateQueryOneOrLess(output3, out, err)
      if not ret_tuple[0]:
        self.query_url_warn = True
        out.write(self.query_url + "foo - WARNING " + ret_tuple[1] + "\n\n")
      else:
        out.write(self.query_url + "foo - PASS\n\n")
      out.write("-----------------------------------------------------\n\n")
      out.flush()

      url4 = self.benchmarker.generate_url(self.query_url + "501", self.port)
      output4 = self.__curl_url(url4, self.QUERY, out, err)
      ret_tuple = self.validateQueryFiveHundredOrMore(output4, out, err)
      if not ret_tuple[0]:
        self.query_url_warn = True
        out.write(self.query_url + "501 - WARNING " + ret_tuple[1] + "\n\n")
      else:
        out.write(self.query_url + "501 - PASS\n\n")
      out.write("-----------------------------------------------------\n\n\n")
      out.flush()

      out.write("VALIDATING QUERY ... ")
      if self.query_url_passed:
        out.write("PASS")
        if self.query_url_warn:
          out.write(" (with warnings)")
        out.write("\n\n")
      else:
        out.write("\nFAIL " + ret_tuple[1] + "\n\n")
        result = False
      out.flush()

    # Fortune
    if self.runTests[self.FORTUNE]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING FORTUNE ({url})
        -----------------------------------------------------
        """.format(url = self.fortune_url)))
      out.flush()

      url = self.benchmarker.generate_url(self.fortune_url, self.port)
      output = self.__curl_url(url, self.FORTUNE, out, err)
      out.write("VALIDATING FORTUNE ... ")
      if self.validateFortune(output, out, err):
        self.fortune_url_passed = True
        out.write("PASS\n\n")
      else:
        self.fortune_url_passed = False
        out.write("\nFAIL\n\n")
        result = False
      out.flush()

    # Update
    if self.runTests[self.UPDATE]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING UPDATE ({url})
        -----------------------------------------------------
        """.format(url = self.update_url)))
      out.flush()

      url = self.benchmarker.generate_url(self.update_url + "2", self.port)
      output = self.__curl_url(url, self.UPDATE, out, err)
      out.write("VALIDATING UPDATE ... ")
      ret_tuple = self.validateUpdate(output, out, err)
      if ret_tuple[0]:
        self.update_url_passed = True
        out.write("PASS\n\n")
      else:
        self.update_url_passed = False
        out.write("\nFAIL " + ret_tuple[1] + "\n\n")
        result = False
      out.flush()

    # plaintext
    if self.runTests[self.PLAINTEXT]:
      out.write(textwrap.dedent("""
        -----------------------------------------------------
          VERIFYING PLAINTEXT ({url})
        -----------------------------------------------------
        """.format(url = self.plaintext_url)))
      out.flush()

      url = self.benchmarker.generate_url(self.plaintext_url, self.port)
      output = self.__curl_url(url, self.PLAINTEXT, out, err)
      out.write("VALIDATING PLAINTEXT ... ")
      ret_tuple = self.validatePlaintext(output, out, err)
      if ret_tuple[0]:
        self.plaintext_url_passed = True
        out.write("PASS\n\n")
      else:
        self.plaintext_url_passed = False
        out.write("\nFAIL\n\n" + ret_tuple[1] + "\n\n")
        result = False
      out.flush()

    return result
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
  def benchmark(self, out, err):
    # JSON
    if self.runTests[self.JSON]:
      try:
        out.write("BENCHMARKING JSON ... ") 
        out.flush()
        results = None
        output_file = self.benchmarker.output_file(self.name, self.JSON)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.json_url_passed:
          remote_script = self.__generate_concurrency_script(self.json_url, self.port, self.accept_json)
          self.__begin_logging(self.JSON)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.JSON)
        print results
        self.benchmarker.report_results(framework=self, test=self.JSON, results=results['results'])
        out.write( "Complete\n" )
        out.flush()
      except AttributeError:
        pass

    # DB
    if self.runTests[self.DB]:
      try:
        out.write("BENCHMARKING DB ... ") 
        out.flush()
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
          self.__begin_logging(self.DB)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.DB)
        self.benchmarker.report_results(framework=self, test=self.DB, results=results['results'])
        out.write( "Complete\n" )
      except AttributeError:
        pass

    # Query
    if self.runTests[self.QUERY]:
      try:
        out.write("BENCHMARKING Query ... ")
        out.flush()
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
          self.__begin_logging(self.QUERY)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.QUERY)
        self.benchmarker.report_results(framework=self, test=self.QUERY, results=results['results'])
        out.write( "Complete\n" )
        out.flush()
      except AttributeError:
        pass

    # fortune
    if self.runTests[self.FORTUNE]:
      try:
        out.write("BENCHMARKING Fortune ... ") 
        out.flush()
        results = None
        output_file = self.benchmarker.output_file(self.name, self.FORTUNE)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.fortune_url_passed:
          remote_script = self.__generate_concurrency_script(self.fortune_url, self.port, self.accept_html)
          self.__begin_logging(self.FORTUNE)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.FORTUNE)
        self.benchmarker.report_results(framework=self, test=self.FORTUNE, results=results['results'])
        out.write( "Complete\n" )
        out.flush()
      except AttributeError:
        pass

    # update
    if self.runTests[self.UPDATE]:
      try:
        out.write("BENCHMARKING Update ... ") 
        out.flush()
        results = None
        output_file = self.benchmarker.output_file(self.name, self.UPDATE)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.update_url_passed:
          remote_script = self.__generate_query_script(self.update_url, self.port, self.accept_json)
          self.__begin_logging(self.UPDATE)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.UPDATE)
        self.benchmarker.report_results(framework=self, test=self.UPDATE, results=results['results'])
        out.write( "Complete\n" )
        out.flush()
      except AttributeError:
        pass

    # plaintext
    if self.runTests[self.PLAINTEXT]:
      try:
        out.write("BENCHMARKING Plaintext ... ")
        out.flush()
        results = None
        output_file = self.benchmarker.output_file(self.name, self.PLAINTEXT)
        if not os.path.exists(output_file):
          with open(output_file, 'w'):
            # Simply opening the file in write mode should create the empty file.
            pass
        if self.plaintext_url_passed:
          remote_script = self.__generate_concurrency_script(self.plaintext_url, self.port, self.accept_plaintext, wrk_command="wrk", intervals=[256,1024,4096,16384], pipeline="16")
          self.__begin_logging(self.PLAINTEXT)
          self.__run_benchmark(remote_script, output_file, err)
          self.__end_logging()
        results = self.__parse_test(self.PLAINTEXT)
        self.benchmarker.report_results(framework=self, test=self.PLAINTEXT, results=results['results'])
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
        json.dump(stats, stats_file)


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
    with open(output_file, 'w') as raw_file:
	  
      p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE, stdout=raw_file, stderr=err)
      p.communicate(script)
      err.flush()
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
  def __curl_url(self, url, testType, out, err):
    output = None
    try:
      # Use -m 15 to make curl stop trying after 15sec.
      # Use -i to output response with headers.
      # Don't use -f so that the HTTP response code is ignored.
      # Use --stderr - to redirect stderr to stdout so we get
      # error output for sure in stdout.
      # Use -sS to hide progress bar, but show errors.
      subprocess.check_call(["curl", "-m", "15", "-i", "-sS", url], stderr=err, stdout=out)
      # HTTP output may not end in a newline, so add that here.
      out.write( "\n\n" )
      out.flush()
      err.flush()

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
      return (self.contains_type(self.FORTUNE) or 
              self.contains_type(self.DB) or 
              self.contains_type(self.QUERY) or
              self.contains_type(self.UPDATE))
  ############################################################
  # __begin_logging
  # Starts a thread to monitor the resource usage, to be synced with the client's time
  # TODO: MySQL and InnoDB are possible. Figure out how to implement them.
  ############################################################
  def __begin_logging(self, test_name):
    output_file = "{file_name}".format(file_name=self.benchmarker.get_stats_file(self.name, test_name))
    dstat_string = "dstat -afilmprsT --aio --fs --ipc --lock --raw --socket --tcp \
                                      --raw --socket --tcp --udp --unix --vm --disk-util \
                                      --rpc --rpcd --output {output_file}".format(output_file=output_file)
    cmd = shlex.split(dstat_string)
    dev_null = open(os.devnull, "w")
    self.subprocess_handle = subprocess.Popen(cmd, stdout=dev_null)
  ##############################################################
  # End __begin_logging
  ##############################################################

  ##############################################################
  # Begin __end_logging
  # Stops the logger thread and blocks until shutdown is complete. 
  ##############################################################
  def __end_logging(self):
    self.subprocess_handle.terminate()
    self.subprocess_handle.communicate()
  ##############################################################
  # End __end_logging
  ##############################################################

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
    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)
    
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
