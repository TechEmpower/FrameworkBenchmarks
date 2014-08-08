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
    mode = [cisetup|prereq|install|verify] for what we want to do
    testdir  = framework directory we are running
    '''

    logging.basicConfig(level=logging.INFO)
    self.directory = testdir
    self.mode = mode

    try:
      # See http://git.io/hs_qRQ
      #   TRAVIS_COMMIT_RANGE is empty for pull requests
      is_pull_req = (os.environ['TRAVIS_PULL_REQUEST'] != "false")
      if is_pull_req:
        self.commit_range = "%s..FETCH_HEAD" % os.environ['TRAVIS_BRANCH']
      else:  
        self.commit_range = os.environ['TRAVIS_COMMIT_RANGE']
    except KeyError:
      log.warning("I should only be used for automated integration tests e.g. Travis-CI")
      log.warning("Were you looking for run-tests.py?")
      last_commit = subprocess.check_output("git rev-parse HEAD^", shell=True).rstrip('\n')
      self.commit_range = "%s...HEAD" % last_commit

    log.info("Using commit range %s", self.commit_range)
    log.info("Running `git diff --name-only %s`" % self.commit_range)
    changes = subprocess.check_output("git diff --name-only %s" % self.commit_range, shell=True)
    log.info(changes)

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
    log.info("Choosing to use test %s to verify directory %s", self.name, testdir)

  def _should_run(self):
    ''' 
    Decides if the current framework test should be tested. 
    Examines git commits included in the latest push to see if any files relevant to 
    this framework were changed. 
    If you do rewrite history (e.g. rebase) then it's up to you to ensure that both 
    old and new (e.g. old...new) are available in the public repository. For simple
    rebase onto the public master this is not a problem, only more complex rebases 
    may have issues
    '''
    # Don't use git diff multiple times, it's mega slow sometimes\
    # Put flag on filesystem so that future calls to run-ci see it too
    if os.path.isfile('.run-ci.should_run'):
      return True
    if os.path.isfile('.run-ci.should_not_run'):
      return False

    def touch(fname):
      open(fname, 'a').close()

    # Look for changes to core TFB framework code
    find_tool_changes = "git diff --name-only %s | grep '^toolset/' | wc -l" % self.commit_range
    changes = subprocess.check_output(find_tool_changes, shell=True)  
    if int(changes) != 0:
      log.info("Found changes to core framework code")
      touch('.run-ci.should_run')
      return True
  
    # Look for changes relevant to this test
    find_test_changes = "git diff --name-only %s | grep '^%s/' | wc -l" % (self.commit_range, self.directory)
    changes = subprocess.check_output(find_test_changes, shell=True)
    if int(changes) == 0:
      log.info("No changes found for %s", self.name)
      touch('.run-ci.should_not_run')
      return False

    log.info("Changes found for %s", self.name)
    touch('.run-ci.should_run')
    return True

  def run(self):
    ''' Do the requested command using TFB  '''

    if not self._should_run():
      log.info("Not running %s", self.name)
      return 0

    if self.mode == 'cisetup':
      self.run_travis_setup()
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
      log.critical("Exception from running+wait on subprocess")
      log.error(err.child_traceback)
      return 1

  def run_travis_setup(self):
    log.info("Setting up Travis-CI")
    
    script = '''
    sudo apt-get update
    sudo apt-get install openssh-server

    # Run as travis user (who already has passwordless sudo)
    ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
    cat /home/travis/.ssh/id_rsa.pub > /home/travis/.ssh/authorized_keys
    chmod 600 /home/travis/.ssh/authorized_keys

    # Setup database manually
    # NOTE: Do not run database installation! It restarts mysql with a different
    # configuration and will break travis's mysql setup
    mysql -uroot < config/create.sql

    # Setup Postgres
    psql --version
    sudo useradd benchmarkdbuser -p benchmarkdbpass
    sudo -u postgres psql template1 < config/create-postgres-database.sql
    sudo -u benchmarkdbuser psql hello_world < config/create-postgres.sql
    '''

    def sh(command):
      log.info("Running `%s`", command)
      subprocess.check_call(command, shell=True)  

    for command in script.split('\n'):
      command = command.lstrip()
      if command != "" and command[0] != '#':
        sh(command.lstrip())

if __name__ == "__main__":
  args = sys.argv[1:]

  usage = '''Usage: toolset/run-ci.py [cisetup|prereq|install|verify] <framework-directory>
    
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

  if len(args) != 2:
    print usage
    sys.exit(1)

  mode = args[0]
  testdir = args[1]
  if len(args) == 2 and (mode == "install" 
    or mode == "verify"
    or mode == 'prereq'
    or mode == 'cisetup'):
    runner = CIRunnner(mode, testdir)
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
    
    # Only print logs if we ran a verify
    if mode != 'verify':
      sys.exit(retcode)

    # Only print logs if we actually did something
    if os.path.isfile('.run-ci.should_not_run'):
      sys.exit(retcode)

    log.error("Running inside Travis-CI, so I will print err and out to console...")
    
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





