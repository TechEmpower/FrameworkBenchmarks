#!/usr/bin/env python

import subprocess
import os
import sys
from benchmark import framework_test
from benchmark.utils import gather_tests
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
  
  def __init__(self, mode, testdir=None):
    '''
    mode = [cisetup|jobcleaner|prereq|install|verify] for what we want to do
    testdir  = framework directory we are running
    '''

    logging.basicConfig(level=logging.INFO)
    self.directory = testdir
    self.name = testdir  # Temporary value, reset below
    self.mode = mode
    self.should_run_cache = None
    self.travis = Travis()

    try:
      # See http://git.io/hs_qRQ
      #   TRAVIS_COMMIT_RANGE is empty for pull requests
      if self.travis.is_pull_req:
        self.commit_range = "%s..FETCH_HEAD" % os.environ['TRAVIS_BRANCH'].rstrip('\n')
      else:  
        self.commit_range = os.environ['TRAVIS_COMMIT_RANGE']
    except KeyError:
      log.warning("I should only be used for automated integration tests e.g. Travis-CI")
      log.warning("Were you looking for run-tests.py?")
      last_commit = subprocess.check_output("git rev-parse HEAD^", shell=True).rstrip('\n')
      self.commit_range = "%s...master" % last_commit

    log.info("Using commit range %s", self.commit_range)
    log.info("Running `git diff --name-only %s`" % self.commit_range)
    changes = subprocess.check_output("git diff --name-only %s" % self.commit_range, shell=True)
    log.info(changes)

    # Nothing else to setup
    if mode == 'cisetup' or mode == 'jobcleaner' or mode == 'prereq':
      return

    # Should we bother to continue
    if not self._should_run():
      return

    #
    # Find the one test from benchmark_config that we are going to run
    #

    tests = gather_tests()
    dirtests = [t for t in tests if t.directory == testdir]
    
    # Travis-CI is linux only
    osvalidtests = [t for t in dirtests if t.os.lower() == "linux"
                  and (t.database_os.lower() == "linux" or t.database_os.lower() == "none")]
    
    # Travis-CI only has some supported databases
    validtests = [t for t in osvalidtests if t.database.lower() == "mysql"
                  or t.database.lower() == "postgres"
                  or t.database.lower() == "none"]
    log.info("Found %s tests (%s for linux, %s for linux and mysql) in directory '%s'", 
      len(dirtests), len(osvalidtests), len(validtests), testdir)
    if len(validtests) == 0:
      log.critical("Found no test that is possible to run in Travis-CI! Aborting!")
      if len(osvalidtests) != 0:
        log.critical("Note: Found these tests that could run in Travis-CI if more databases were supported")
        log.criticat("Note: %s", osvalidtests)
      sys.exit(1)
    
    # Prefer database tests over 'none' if we have both
    preferred = [t for t in validtests if t.database.lower() != "none"]
    if len(preferred) > 0:
      self.test = preferred[0]
    else:
      self.test = validtests[0]
    self.name = self.test.name
    log.info("Choosing to run test %s in %s", self.name, testdir)

  def _should_run(self):
    ''' 
    Decides if the current framework test should be tested or if we can cancel it.
    Examines git commits included in the latest push to see if any files relevant to 
    this framework were changed. 
    This is a rather primitive strategy for things like pull requests, where
    we probably want to examine the entire branch of commits. Also, this cannot handle 
    history re-writing very well, so avoid rebasing onto any published history
    '''
    
    # Don't use git diff twice, it's mega slow sometimes
    if self.should_run_cache is not None: 
      return self.should_run_cache

    # Look for changes to core TFB framework code
    find_tool_changes = "git diff --name-only %s | grep '^toolset/' | wc -l" % self.commit_range
    changes = subprocess.check_output(find_tool_changes, shell=True)  
    if int(changes) != 0:
      log.info("Found changes to core framework code")
      self.should_run_cache = True
      return True
  
    # Look for changes relevant to this test
    find_test_changes = "git diff --name-only %s | grep '^%s/' | wc -l" % (self.commit_range, self.directory)
    changes = subprocess.check_output(find_test_changes, shell=True)
    if int(changes) == 0:
      log.info("No changes found for %s", self.name)
      self.should_run_cache = False
      return False

    log.info("Changes found for %s", self.name)
    self.should_run_cache = True
    return True

  def run(self):
    ''' Do the requested command using TFB  '''

    if self.mode == 'jobcleaner':
      self.cancel_unneeded_jobs()
      return 0

    if self.mode == 'cisetup' and self._should_run():
      self.run_travis_setup()
      return 0

    if not self._should_run():
      log.info("Not running %s", self.name)
      
      # Cancel ourselves
      self.travis.cancel(self.travis.jobid)
      return 0

    command = 'toolset/run-tests.py '
    if self.mode == 'prereq':
      command = command + "--install server --install-only --test ''"
    elif self.mode == 'install':
      command = command + "--install server --install-only --test %s" % self.name
    elif self.mode == 'verify':
      command = command + "--mode verify --test %s" % self.name
    else:
      log.critical('Unknown mode passed')
      return 1
    
    # Run the command
    log.info("Running mode %s with commmand %s", self.mode, command)
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

  def run_travis_setup(self):
    print "running travis setup"

  def cancel_unneeded_jobs(self):
    log.info("I am jobcleaner")
    log.info("Sleeping to ensure Travis-CI has queued all jobs")
    time.sleep(20)

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
    self.is_pull_req = (os.environ['TRAVIS_PULL_REQUEST'] != "false")
    self.logged_in = False

  def _login(self):
    if self.logged_in:
      return

    # If this is a PR, we cannot access the secure variable 
    # GH_TOKEN, and instead must return success for all jobs
    if not self.is_pull_req:
      self.token = os.environ['GH_TOKEN']
      subprocess.check_call("travis login --skip-version-check --no-interactive --github-token %s" % self.token, shell=True)
      log.info("Logged into travis") # NEVER PRINT OUTPUT, GH_TOKEN MIGHT BE REVEALED      
    else:
      log.info("Pull Request Detected. Non-necessary jobs will return pass instead of being canceled")

    self.logged_in = True

  def cancel(self, job):
    self._login()

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
    self._login()

    # If this is a pull request, we cannot interact with the CLI
    if self.is_pull_req:
      return "No details available"

    build = subprocess.check_output("travis show %s --skip-version-check" % self.buildid, shell=True)
    return build

if __name__ == "__main__":
  args = sys.argv[1:]

  usage = '''Usage: toolset/run-ci.py [cisetup|prereq]
    OR toolset/run-ci.py [install|verify] <framework-directory>

    run-ci.py selects one test from <framework-directory>/benchark_config, and 
    automates a number of calls into run-tests.py specific to the selected test. 

    It is guaranteed to always select the same test from the benchark_config, so 
    multiple runs with the same <framework-directory> reference the same test. 
    The name of the selected test will be printed to standard output. 

    cisetup - configure the Travis-CI environment for our test suite
    prereq  - trigger standard prerequisite installation
    install - trigger server installation for the selected test_directory
    verify  - run a verification on the selected test using `--mode verify`

    run-ci.py expects to be run inside the Travis-CI build environment, and 
    will expect environment variables such as $TRAVIS_BUILD'''

  mode = args[0]
  if mode == 'cisetup' or mode == 'prereq':
    runner = CIRunnner(mode)
  elif len(args) == 2 and args[1] == 'jobcleaner':
    # Only run jobcleaner once
    if mode != 'verify':
      sys.exit(0)

    # Translate jobcleaner from a directory name to a mode
    mode = 'jobcleaner'
    runner = CIRunnner(args[1])
  elif len(args) == 2 and (mode == "install" 
    or mode == "verify"):
    runner = CIRunnner(mode, args[1])
  else:
    print usage
    sys.exit(1)
    
  retcode = 0
  try:
    retcode = runner.run()
  except KeyError as ke: 
    log.warning("Environment key missing, are you running inside Travis-CI?")
    print traceback.format_exc()
  except:
    log.critical("Unknown error")
    print traceback.format_exc()
  finally:  # Ensure that logs are printed
    
    # Only print logs if we are not jobcleaner and we ran a verify
    if mode == 'jobcleaner' or mode != 'verify':
      sys.exit(retcode)          

    log.error("Running inside travis, so I will print err and out to console")
    
    try:
      log.error("Here is ERR:")
      with open("results/ec2/latest/logs/%s/err.txt" % runner.test.name, 'r') as err:
        for line in err:
          log.info(line)
    except IOError:
      log.error("No ERR file found")

    try:
      log.error("Here is OUT:")
      with open("results/ec2/latest/logs/%s/out.txt" % runner.test.name, 'r') as out:
        for line in out:
          log.info(line)
    except IOError:
      log.error("No OUT file found")

    sys.exit(retcode)





