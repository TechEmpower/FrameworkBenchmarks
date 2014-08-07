#!/usr/bin/env python

import subprocess
import os
import sys
from benchmark import framework_test
import glob
import json
import traceback
import re
import logging
log = logging.getLogger('run-ci')
import time
import threading

# Needed for various imports
sys.path.append('.')
sys.path.append('toolset/setup/linux')
sys.path.append('toolset/benchmark')

class CIRunnner:
  '''
  Manages running TFB on the Travis Continuous Integration system. 
  Makes a best effort to avoid wasting time and resources by running 
  useless jobs. 
  
  Only verifies the first test in each directory 
  '''
  
  def __init__(self, mode, test_directory):
    '''
    mode = [prereq|install|test] for what we want TFB to do
    dir  = directory we are running
    '''

    logging.basicConfig(level=logging.INFO)
    
    if not test_directory == 'jobcleaner':
      tests = self.gather_tests()
      
      # Run the first linux-only test in this directory
      # At the moment, travis only supports mysql!
      dirtests = [t for t in tests if t.directory == test_directory]
      osvalidtests = [t for t in dirtests if t.os.lower() == "linux"
                    and (t.database_os.lower() == "linux" or t.database_os.lower() == "none")]
      validtests = [t for t in osvalidtests if t.database.lower() == "mysql"
                    or t.database.lower() == "none"]
      log.info("Found %s tests (%s for linux, %s for linux and mysql) in directory '%s'", 
        len(dirtests), len(osvalidtests), len(validtests), test_directory)
      if len(validtests) == 0:
        log.critical("Found no test that is possible to run in Travis-CI! Aborting!")
        if len(osvalidtests) != 0:
          log.critical("Note: Found tests that could run in Travis-CI if more databases were supported")
        sys.exit(1)
      self.test = validtests[0]
      self.name = self.test.name
      log.info("Choosing to run test %s in %s", self.name, test_directory)

    self.mode = mode
    self.travis = Travis()

    try:
      # See http://git.io/hs_qRQ
      #   TRAVIS_COMMIT_RANGE is empty for pull requests
      if self.travis.is_pull_req:
        self.commit_range = "%s..FETCH_HEAD" % os.environ['TRAVIS_BRANCH'].rstrip('\n')
      else:  
        self.commit_range = os.environ['TRAVIS_COMMIT_RANGE']
    except KeyError:
      log.warning("Run-ci.py should only be used for automated integration tests")
      last_commit = subprocess.check_output("git rev-parse HEAD^", shell=True).rstrip('\n')
      self.commit_range = "master...%s" % last_commit

    log.info("Using commit range %s", self.commit_range)
    log.info("Running `git diff --name-only %s`" % self.commit_range)
    changes = subprocess.check_output("git diff --name-only %s" % self.commit_range, shell=True)
    log.info(changes)

  def _should_run(self):
    ''' 
    Decides if the current framework test should be tested or if we can cancel it.
    Examines git commits included in the latest push to see if any files relevant to 
    this framework were changed. 
    This is a rather primitive strategy for things like pull requests, where
    we probably want to examine the entire branch of commits. Also, this cannot handle 
    history re-writing very well, so avoid rebasing onto any published history
    '''

    # Look for changes to core TFB framework code
    find_tool_changes = "git diff --name-only %s | grep '^toolset/' | wc -l" % self.commit_range
    changes = subprocess.check_output(find_tool_changes, shell=True)  
    if int(changes) != 0:
      log.info("Found changes to core framework code")
      return True
  
    # Look for changes relevant to this test
    find_test_changes = "git diff --name-only %s | grep '^%s/' | wc -l" % (self.commit_range, self.test.directory)
    changes = subprocess.check_output(find_test_changes, shell=True)
    if int(changes) == 0:
      log.info("No changes found for %s", self.name)
      return False

    log.info("Changes found for %s", self.name)
    return True

  def run(self):
    ''' Do the requested command using TFB  '''
    if not self._should_run():
      log.info("Not running %s", self.name)
      
      # Cancel ourselves
      self.travis.cancel(self.travis.jobid)
      return 0

    log.info("Running %s for %s", self.mode, self.name)
    
    command = 'toolset/run-tests.py '
    if mode == 'prereq':
      command = command + "--install server --test ''"
    elif mode == 'install':
      # Just a note that having an install-only mode would integrate nicely with 
      # Travis-CI's line-folding
      log.warning('Currently there is no install-only mode available')
      return 1
    elif mode == 'test':
      command = command + "--install server --mode verify --test %s" % self.name
    else:
      log.critical('Unknown mode passed')
      return 1
    
    # Run the command
    log.info("Running %s", command)
    try:
      p = subprocess.Popen(command, shell=True)
      p.wait()
      return p.returncode  
    except subprocess.CalledProcessError:
      log.critical("Subprocess Error")
      print traceback.format_exc()
      return 1
    except Exception as err:
      log.critical("Subprocess Error")
      log.error(err.child_traceback)
      return 1

  def gather_tests(self):
    ''' Returns all available tests as FrameworkTest list '''

    # Fake benchmarker fields that are used
    class bench_shim():
      def __init__(self):
        self.type = 'all'
        self.fwroot = os.getcwd()
        self.install_strategy='pertest'

    # Gather all tests
    tests = []
    for config_file_name in glob.glob('*/benchmark_config'):
      with open(config_file_name, 'r') as config_file:
        config = json.load(config_file)
        test = framework_test.parse_config(config, os.path.dirname(config_file_name), bench_shim())
        tests = tests + test
    tests.sort(key=lambda x: x.name)
    return tests

  def cancel_unneeded_jobs(self):
    log.info("I am jobcleaner")

    # Look for changes to core TFB framework code
    find_tool_changes = "git diff --name-only %s | grep toolset | wc -l" % self.commit_range
    changes = subprocess.check_output(find_tool_changes, shell=True)  
    if int(changes) != 0:
      log.info("Found changes to core framework code. Running all tests")
      self.travis.cancel(self.travis.jobid) # Cancel ourselves
      return 0
    
    build = self.travis.build_details()
    log.info("Build details:\n%s", build)
    def parse_job_id(directory):
      for line in build.split('\n'):
        if "TESTDIR=%s" % directory in line: 
          job = re.findall("\d+.\d+", line)[0]
          return job
    
    # Build a list of modified directories
    changes = subprocess.check_output("git diff --name-only %s" % self.commit_range, shell=True)
    dirchanges = []
    for line in changes.split('\n'):
      dirchanges.append(line[0:line.find('/')])

    # For each test, launch a Thread to cancel it's job if 
    # it's directory has not been modified
    cancelled_testdirs = []
    threads = []
    for test in self.gather_tests():
      if test.directory not in dirchanges:
        job = parse_job_id(test.directory)
        log.info("No changes found for %s (job=%s) (dir=%s)", test.name, job, test.directory)
        if job and test.directory not in cancelled_testdirs:
          cancelled_testdirs.append(test.directory)
          t = threading.Thread(target=self.travis.cancel, args=(job,),
            name="%s (%s)" % (job, test.name))
          t.start()
          threads.append(t)

    # Wait for all threads
    for t in threads:
      t.join()

    # Cancel ourselves
    self.travis.cancel(self.travis.jobid)


class Travis():
  '''Integrates the travis-ci build environment and the travis command line'''
  def __init__(self):     
    self.jobid = os.environ['TRAVIS_JOB_NUMBER']
    self.buildid = os.environ['TRAVIS_BUILD_NUMBER']
    self.is_pull_req = "false" != os.environ['TRAVIS_PULL_REQUEST']

    # If this is a PR, we cannot access the secure variable 
    # GH_TOKEN, and instead must return success for all jobs
    if not self.is_pull_req:
      self.token = os.environ['GH_TOKEN']
      self._login()
    else:
      log.info("Pull Request Detected. Non-necessary jobs will return pass instead of being canceled")

  def _login(self):
    subprocess.check_call("travis login --skip-version-check --no-interactive --github-token %s" % self.token, shell=True)
    log.info("Logged into travis") # NEVER PRINT OUTPUT, GH_TOKEN MIGHT BE REVEALED

  def cancel(self, job):
    # If this is a pull request, we cannot interact with the CLI
    if self.is_pull_req:
      log.info("Thread %s: Return pass for job %s", threading.current_thread().name, job)
      return

    # Ignore errors in case job is already cancelled
    try:
      subprocess.check_call("travis cancel %s --skip-version-check --no-interactive" % job, shell=True)
      log.info("Thread %s: Canceled job %s", threading.current_thread().name, job)
    except subprocess.CalledProcessError:
      log.exception("Error halting job %s. Report:", job)
      subprocess.call("travis report --skip-version-check --no-interactive --org", shell=True)
      log.error("Trying to halt %s one more time", job)
      subprocess.call("travis cancel %s --skip-version-check --no-interactive" % job, shell=True)

  def build_details(self):
    # If this is a pull request, we cannot interact with the CLI
    if self.is_pull_req:
      return "No details available"

    build = subprocess.check_output("travis show %s --skip-version-check" % self.buildid, shell=True)
    return build

if __name__ == "__main__":
  args = sys.argv[1:]

  if len(args) != 2 or not (args[0] == "prereq" or args[0] == "install" or args[0] == "test"):
    print "Usage: toolset/run-ci.py [prereq|install|test] test-name"
    sys.exit(1)

  mode = args[0]
  testdir = args[1]

  runner = CIRunnner(mode, testdir)
  
  # Watch for the special test name indicating we are 
  # the test in charge of cancelling unnecessary jobs
  if testdir == "jobcleaner":
    try:
      log.info("Sleeping to ensure Travis-CI has queued all jobs")
      time.sleep(20)
      runner.cancel_unneeded_jobs()
    except KeyError as ke: 
      log.warning("Environment key missing, are you running inside Travis-CI?")
    except:
      log.critical("Unknown error")
      print traceback.format_exc()
    finally: 
      sys.exit(0)
  
  retcode = 0
  try:
    retcode = runner.run()
  except KeyError as ke: 
    log.warning("Environment key missing, are you running inside Travis-CI?")
  except:
    log.critical("Unknown error")
    print traceback.format_exc()
  finally:
    log.error("Running inside travis, so I will print err and out to console")
    log.error("Here is ERR:")
    
    try:
      with open("results/ec2/latest/logs/%s/err.txt" % runner.test.name, 'r') as err:
        for line in err:
          log.info(line)
    except IOError:
      if mode == "test":
        log.error("No ERR file found")

    log.error("Here is OUT:")
    try:
      with open("results/ec2/latest/logs/%s/out.txt" % runner.test.name, 'r') as out:
        for line in out:
          log.info(line)
    except IOError:
      if mode == "test":
        log.error("No OUT file found")

    sys.exit(retcode)





