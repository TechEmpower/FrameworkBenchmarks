#!/usr/bin/env python

import subprocess
import os
import sys
import glob
import json
import traceback
import re
import logging
log = logging.getLogger('run-ci')
import time
import threading
from benchmark import framework_test
from benchmark.utils import gather_tests
from benchmark.utils import header

# Cross-platform colored text
from colorama import Fore, Back, Style

# Needed for various imports
sys.path.append('.')
sys.path.append('toolset/setup/linux')
sys.path.append('toolset/benchmark')

from setup.linux import setup_util

class CIRunnner:
  '''
  Manages running TFB on the Travis Continuous Integration system. 
  Makes a best effort to avoid wasting time and resources by running 
  useless jobs. 
  
  Only verifies the first test in each directory 
  '''

  SUPPORTED_DATABASES = "mysql postgres mongodb cassandra elasticsearch sqlite none".split()
  
  def __init__(self, mode, testdir=None):
    '''
    mode = [cisetup|prereq|install|verify] for what we want to do
    testdir  = framework directory we are running
    '''

    self.directory = testdir
    self.mode = mode
    if mode == "cisetup":
      logging.basicConfig(level=logging.DEBUG)
    else:
      logging.basicConfig(level=logging.INFO)

    try:
      # NOTE: THIS IS VERY TRICKY TO GET RIGHT!
      #
      # Our goal: Look at the files changed and determine if we need to 
      # run a verification for this folder. For a pull request, we want to 
      # see the list of files changed by any commit in that PR. For a 
      # push to master, we want to see a list of files changed by the pushed
      # commits. If this list of files contains the current directory, or 
      # contains the toolset/ directory, then we need to run a verification
      # 
      # If modifying, please consider: 
      #  - the commit range for a pull request is the first PR commit to 
      #    the github auto-merge commit
      #  - the commits in the commit range may include merge commits
      #    other than the auto-merge commit. An git log with -m 
      #    will know that *all* the files in the merge were changed, 
      #    but that is not the changeset that we care about
      #  - git diff shows differences, but we care about git log, which
      #    shows information on what was changed during commits
      #  - master can (and will!) move during a build. This is one 
      #    of the biggest problems with using git diff - master will 
      #    be updated, and those updates will include changes to toolset, 
      #    and suddenly every job in the build will start to run instead 
      #    of fast-failing
      #  - commit_range is not set if there was only one commit pushed, 
      #    so be sure to test for that on both master and PR
      #  - commit_range and commit are set very differently for pushes
      #    to an owned branch versus pushes to a pull request, test
      #  - For merge commits, the TRAVIS_COMMIT and TRAVIS_COMMIT_RANGE 
      #    will become invalid if additional commits are pushed while a job is 
      #    building. See https://github.com/travis-ci/travis-ci/issues/2666
      #  - If you're really insane, consider that the last commit in a 
      #    pull request could have been a merge commit. This means that 
      #    the github auto-merge commit could have more than two parents
      #  - Travis cannot really support rebasing onto an owned branch, the
      #    commit_range they provide will include commits that are non-existant
      #    in the repo cloned on the workers. See https://github.com/travis-ci/travis-ci/issues/2668
      #  
      #  - TEST ALL THESE OPTIONS: 
      #      - On a branch you own (e.g. your fork's master)
      #          - single commit
      #          - multiple commits pushed at once
      #          - commit+push, then commit+push again before the first
      #            build has finished. Verify all jobs in the first build 
      #            used the correct commit range
      #          - multiple commits, including a merge commit. Verify that
      #            the unrelated merge commit changes are not counted as 
      #            changes the user made
      #      - On a pull request
      #          - repeat all above variations
      #
      #
      # ==== CURRENT SOLUTION FOR PRs ====
      #
      # For pull requests, we will examine Github's automerge commit to see
      # what files would be touched if we merged this into the current master. 
      # You can't trust the travis variables here, as the automerge commit can
      # be different for jobs on the same build. See https://github.com/travis-ci/travis-ci/issues/2666
      # We instead use the FETCH_HEAD, which will always point to the SHA of 
      # the lastest merge commit. However, if we only used FETCH_HEAD than any
      # new commits to a pull request would instantly start affecting currently
      # running jobs and the the list of changed files may become incorrect for
      # those affected jobs. The solution is to walk backward from the FETCH_HEAD
      # to the last commit in the pull request. Based on how github currently 
      # does the automerge, this is the second parent of FETCH_HEAD, and 
      # therefore we use FETCH_HEAD^2 below
      #
      # This may not work perfectly in situations where the user had advanced 
      # merging happening in their PR. We correctly handle them merging in 
      # from upstream, but if they do wild stuff then this will likely break
      # on that. However, it will also likely break by seeing a change in 
      # toolset and triggering a full run when a partial run would be 
      # acceptable
      #
      # ==== CURRENT SOLUTION FOR OWNED BRANCHES (e.g. master) ====
      #
      # This one is fairly simple. Find the commit or commit range, and 
      # examine the log of files changes. If you encounter any merges, 
      # then fully explode the two parent commits that made the merge
      # and look for the files changed there. This is an aggressive 
      # strategy to ensure that commits to master are always tested 
      # well
      log.debug("TRAVIS_COMMIT_RANGE: %s", os.environ['TRAVIS_COMMIT_RANGE'])
      log.debug("TRAVIS_COMMIT      : %s", os.environ['TRAVIS_COMMIT'])

      is_PR = (os.environ['TRAVIS_PULL_REQUEST'] != "false")
      if is_PR:
        log.debug('I am testing a pull request')
        first_commit = os.environ['TRAVIS_COMMIT_RANGE'].split('...')[0]
        last_commit = subprocess.check_output("git rev-list -n 1 FETCH_HEAD^2", shell=True).rstrip('\n')
        log.debug("Guessing that first commit in PR is : %s", first_commit)
        log.debug("Guessing that final commit in PR is : %s", last_commit)

        if first_commit == "":
          # Travis-CI is not yet passing a commit range for pull requests
          # so we must use the automerge's changed file list. This has the 
          # negative effect that new pushes to the PR will immediately 
          # start affecting any new jobs, regardless of the build they are on
          log.debug("No first commit, using Github's automerge commit")
          self.commit_range = "--first-parent -1 -m FETCH_HEAD"
        elif first_commit == last_commit:
          # There is only one commit in the pull request so far, 
          # or Travis-CI is not yet passing the commit range properly 
          # for pull requests. We examine just the one commit using -1
          #
          # On the oddball chance that it's a merge commit, we pray  
          # it's a merge from upstream and also pass --first-parent 
          log.debug("Only one commit in range, examining %s", last_commit)
          self.commit_range = "-m --first-parent -1 %s" % last_commit
        else: 
          # In case they merged in upstream, we only care about the first 
          # parent. For crazier merges, we hope
          self.commit_range = "--first-parent %s...%s" % (first_commit, last_commit)

      if not is_PR:
        log.debug('I am not testing a pull request')
        # Three main scenarios to consider
        #  - 1 One non-merge commit pushed to master
        #  - 2 One merge commit pushed to master (e.g. a PR was merged). 
        #      This is an example of merging a topic branch
        #  - 3 Multiple commits pushed to master
        # 
        #  1 and 2 are actually handled the same way, by showing the 
        #  changes being brought into to master when that one commit 
        #  was merged. Fairly simple, `git log -1 COMMIT`. To handle 
        #  the potential merge of a topic branch you also include 
        #  `--first-parent -m`. 
        #
        #  3 needs to be handled by comparing all merge children for 
        #  the entire commit range. The best solution here would *not* 
        #  use --first-parent because there is no guarantee that it 
        #  reflects changes brought into master. Unfortunately we have
        #  no good method inside Travis-CI to easily differentiate 
        #  scenario 1/2 from scenario 3, so I cannot handle them all 
        #  separately. 1/2 are the most common cases, 3 with a range 
        #  of non-merge commits is the next most common, and 3 with 
        #  a range including merge commits is the least common, so I 
        #  am choosing to make our Travis-CI setup potential not work 
        #  properly on the least common case by always using 
        #  --first-parent 
        
        # Handle 3
        # Note: Also handles 2 because Travis-CI sets COMMIT_RANGE for 
        # merged PR commits
        self.commit_range = "--first-parent -m %s" % os.environ['TRAVIS_COMMIT_RANGE']

        # Handle 1
        if self.commit_range == "":
          self.commit_range = "--first-parent -m -1 %s" % os.environ['TRAVIS_COMMIT']

    except KeyError:
      log.warning("I should only be used for automated integration tests e.g. Travis-CI")
      log.warning("Were you looking for run-tests.py?")
      self.commit_range = "-m HEAD^...HEAD"

    #
    # Find the one test from benchmark_config.json that we are going to run
    #

    tests = gather_tests()
    self.fwroot = setup_util.get_fwroot()
    target_dir = self.fwroot + '/frameworks/' + testdir
    log.debug("Target directory is %s", target_dir)
    dirtests = [t for t in tests if t.directory == target_dir]
    
    # Travis-CI is linux only
    osvalidtests = [t for t in dirtests if t.os.lower() == "linux"
                  and (t.database_os.lower() == "linux" or t.database_os.lower() == "none")]
    
    # Our Travis-CI only has some databases supported
    validtests = [t for t in osvalidtests if t.database.lower() in self.SUPPORTED_DATABASES]
    supported_databases = ','.join(self.SUPPORTED_DATABASES)
    log.info("Found %s usable tests (%s valid for linux, %s valid for linux and {%s}) in directory '%s'",
      len(dirtests), len(osvalidtests), len(validtests), supported_databases, '$FWROOT/frameworks/' + testdir)
    if len(validtests) == 0:
      log.critical("Found no test that is possible to run in Travis-CI! Aborting!")
      if len(osvalidtests) != 0:
        log.critical("Note: Found these tests that could run in Travis-CI if more databases were supported")
        log.critical("Note: %s", osvalidtests)
        databases_needed = [t.database for t in osvalidtests]
        databases_needed = list(set(databases_needed))
        log.critical("Note: Here are the needed databases:")
        log.critical("Note: %s", databases_needed)
      sys.exit(1)

    self.names = [t.name for t in validtests]
    log.info("Using tests %s to verify directory %s", self.names, '$FWROOT/frameworks/' + testdir)

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

    log.debug("Using commit range `%s`", self.commit_range)
    log.debug("Running `git log --name-only --pretty=\"format:\" %s`" % self.commit_range)
    changes = ""
    try:
      changes = subprocess.check_output("git log --name-only --pretty=\"format:\" %s" % self.commit_range, shell=True)
    except subprocess.CalledProcessError, e:
      log.error("Got errors when using git to detect your changes, assuming that we must run this verification!")
      log.error("Error was: %s", e.output)
      log.error("Did you rebase a branch? If so, you can safely disregard this error, it's a Travis limitation")
      return True
    changes = os.linesep.join([s for s in changes.splitlines() if s]) # drop empty lines
    if len(changes.splitlines()) > 1000:
      log.debug("Change list is >1000 lines, uploading to sprunge.us instead of printing to console")
      url = subprocess.check_output("git log --name-only %s | curl -F 'sprunge=<-' http://sprunge.us" % self.commit_range, shell=True)
      log.debug("Uploaded to %s", url)
    else:
      log.debug("Result:\n%s", changes)

    # Look for changes to core TFB framework code
    if re.search(r'^toolset/', changes, re.M) is not None: 
      log.info("Found changes to core framework code")
      touch('.run-ci.should_run')
      return True
  
    # Look for changes relevant to this test
    if re.search("^frameworks/%s/" % re.escape(self.directory), changes, re.M) is None:
      log.info("No changes found for directory %s", self.directory)
      touch('.run-ci.should_not_run')
      return False

    log.info("Changes found for directory %s", self.directory)
    touch('.run-ci.should_run')
    return True

  def run(self):
    ''' Do the requested command using TFB  '''

    if not self._should_run():
      log.info("I found no changes to `%s` or `toolset/`, aborting verification", self.directory)
      return 0

    if self.mode == 'cisetup':
      self.run_travis_setup()
      return 0

    names = ' '.join(self.names)
    command = 'toolset/run-tests.py '
    if self.mode == 'prereq':
      command = command + "--install server --install-only --test '' --verbose"
    elif self.mode == 'install':
      command = command + "--install server --install-only --test %s" % names
    elif self.mode == 'verify':
      command = command + "--mode verify --test %s" % names
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
    export DEBIAN_FRONTEND=noninteractive

    # Turn on command tracing
    set -x 

    # Setup Apt For MongoDB
    #   Due to TechEmpower/FrameworkBenchmarks#989 and travis-ci/travis-ci#2655, 
    #   we put this into a loop
    until timeout 15s sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10; do echo 'Waiting for apt-key' ; done
    echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list

    # Setup apt for Apache Cassandra
    until timeout 15s sudo apt-key adv --keyserver pgp.mit.edu --recv 4BD736A82B5C1B00; do echo 'Waiting for apt-key' ; done
    sudo apt-add-repository  'deb http://www.apache.org/dist/cassandra/debian 20x main'

    # Run installation 
    # DO NOT COPY --force-yes TO ANY NON-TRAVIS-CI SCRIPTS! Seriously, it can cause some 
    # major damage and should only be used inside a VM or Linux Container
    sudo apt-get -q update
    sudo apt-get -q -y --force-yes install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
      mongodb-org \
      cassandra \
      openssh-server

    # Run as travis user (who already has passwordless sudo)
    ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
    cat /home/travis/.ssh/id_rsa.pub > /home/travis/.ssh/authorized_keys
    chmod 600 /home/travis/.ssh/authorized_keys

    # Set up the benchmark.cfg for travis user
    # NOTE: Please don't just copy the example config - it causes unexpected
    #       issues when those example variables change
    echo "[Defaults]"                                       > benchmark.cfg
    echo "client_identity_file=/home/travis/.ssh/id_rsa"   >> benchmark.cfg
    echo "database_identity_file=/home/travis/.ssh/id_rsa" >> benchmark.cfg
    echo "client_host=127.0.0.1"                           >> benchmark.cfg
    echo "database_host=127.0.0.1"                         >> benchmark.cfg
    echo "server_host=127.0.0.1"                           >> benchmark.cfg
    echo "client_user=travis"                              >> benchmark.cfg
    echo "database_user=travis"                            >> benchmark.cfg
    echo "runner_user=testrunner"                          >> benchmark.cfg

    # Create the new testrunner user
    sudo useradd testrunner
    # Give him a home dir
    sudo mkdir /home/testrunner
    # Make testrunner the owner of his home dir
    sudo chown testrunner:testrunner /home/testrunner
    # Add the testrunner user to every group that the travis user is in
    sudo sed -i 's|:travis|:travis,testrunner|g' /etc/group
    # Add the testrunner user to the travis group specifically
    sudo sed -i 's|travis:x:\(.*\):|travis:x:\\1:testrunner|g' /etc/group
    # Maybe unneeded - add the travis user to the testrunner group
    sudo sed -i 's|testrunner:x:\(.*\):|testrunner:x:\\1:travis|g' /etc/group
    # Need to add testrunner to the sudoers group AND default him to a sudoers
    # because the travis user isn't in the sudo group - he's a sudoer.
    echo "testrunner ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers
    # Set the default shell for testrunner to /bin/bash
    sudo sed -i 's|/home/testrunner:/bin/sh|/home/testrunner:/bin/bash|g' /etc/passwd

    # =============Setup Databases===========================
    # NOTE: Do not run `--install database` in travis-ci! 
    #       It changes DB configuration files and will break everything
    # =======================================================

    # Setup MySQL
    echo "Populating MySQL database"
    mysql -uroot < config/create.sql

    # Setup Postgres
    echo "Removing Postgres 9.1 from Travis-CI"
    sudo apt-get remove -qy postgresql postgresql-9.1 postgresql-client-9.1
    sudo apt-get install -qy postgresql-9.3 postgresql-client-9.3

    echo "Populating Postgres database"
    psql --version
    sudo useradd benchmarkdbuser -p benchmarkdbpass
    sudo -u postgres psql template1 < config/create-postgres-database.sql
    sudo -u benchmarkdbuser psql hello_world < config/create-postgres.sql

    # Setup Apache Cassandra
    echo "Populating Apache Cassandra database"
    for i in {1..15}; do
      nc -z localhost 9160 && break || sleep 1;
      echo "Waiting for Cassandra ($i/15}"
    done
    nc -z localhost 9160
    if [ $? -eq 0 ]; then
      cat config/cassandra/cleanup-keyspace.cql | sudo cqlsh
      python config/cassandra/db-data-gen.py > config/cassandra/tfb-data.cql
      sudo cqlsh -f config/cassandra/create-keyspace.cql
      sudo cqlsh -f config/cassandra/tfb-data.cql
    else
      >&2 echo "Cassandra did not start, skipping"
    fi

    # Setup Elasticsearch
    curl -O https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.5.0.deb
    sudo dpkg -i --force-confnew elasticsearch-1.5.0.deb
    sudo update-rc.d elasticsearch defaults 95 10
    sudo service elasticsearch restart

    echo "Populating Elasticsearch database"
    for i in {1..15}; do
      nc -z localhost 9200 && break || sleep 1;
      echo "Waiting for Elasticsearch ($i/15}"
    done
    nc -z localhost 9200
    if [ $? -eq 0 ]; then
      curl localhost:9200
      sh config/elasticsearch/es-create-index.sh
      python config/elasticsearch/es-db-data-gen.py > config/elasticsearch/tfb-data.json
      curl -sS -D - -o /dev/null -XPOST localhost:9200/tfb/world/_bulk --data-binary @config/elasticsearch/tfb-data.json
      echo "Elasticsearch DB populated"
    else
      >&2 echo "Elasticsearch did not start, skipping"
    fi

    # Setup MongoDB
    echo "Populating MongoDB database"
    for i in {1..15}; do
      nc -z localhost 27017 && break || sleep 1;
      echo "Waiting for MongoDB ($i/15}"
    done
    nc -z localhost 27017
    if [ $? -eq 0 ]; then
      mongo < config/create.js
      mongod --version
    else
      >&2 echo "MongoDB did not start, skipping"
    fi
    
    # =============Modify Configurations===========================
    # It can be useful to enable debug features for verification 
    # inside Travis-CI
    # =======================================================

    sed -i 's|display_errors\] = off|display_errors\] = on|' config/php-fpm.conf
    
    exit $?
    '''

    p = subprocess.Popen(["bash"], stdin=subprocess.PIPE)
    p.communicate(script)
    if p.wait() != 0:
      log.critical("Non-zero exit  from running+wait on subprocess")

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
    retcode = 1
  except Exception:
    log.critical("Unknown error")
    print traceback.format_exc()
    retcode = 1
  finally:
    sys.exit(retcode)

# vim: set sw=2 ts=2 expandtab
