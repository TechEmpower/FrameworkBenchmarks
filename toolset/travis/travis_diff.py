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
from sets import Set

# Returns a unique list of fw_depends changes
def get_fw_depends_changes(changes_output):
    return list(Set(re.findall(r"toolset/setup/linux/.+/(.+)\.sh", changes_output, re.M)))

# Returns a unique list of frameworks that have been changed
def get_fw_changes(changes_output):
    return list(Set(re.findall(r"frameworks/.+/(.+)/", changes_output, re.M)))

# Cleans up diffing and grep output and into an array of strings
def clean_output(output):
    return os.linesep.join([s for s in output.splitlines() if s]) # drop empty lines


log.debug("TRAVIS_COMMIT_RANGE: %s", os.environ['TRAVIS_COMMIT_RANGE'])
log.debug("TRAVIS_COMMIT      : %s", os.environ['TRAVIS_COMMIT'])

is_PR = (os.environ['TRAVIS_PULL_REQUEST'] != "false")
commit_range = ""

if is_PR:
    log.debug('I am testing a pull request')
    first_commit = os.environ['TRAVIS_COMMIT_RANGE'].split('...')[0]
    last_commit = subprocess.check_output("git rev-list -n 1 FETCH_HEAD^2", shell=True).rstrip('\n')
    log.debug("Guessing that first commit in PR is : %s", first_commit)
    log.debug("Guessing that final commit in PR is : %s", last_commit)

    if first_commit == "":
        log.debug("No first commit, using Github's automerge commit")
        commit_range = "--first-parent -1 -m FETCH_HEAD"
    elif first_commit == last_commit:
        log.debug("Only one commit in range, examining %s", last_commit)
        commit_range = "-m --first-parent -1 %s" % last_commit
    else:
        commit_range = "--first-parent %s...%s" % (first_commit, last_commit)

if not is_PR:
    log.debug('I am not testing a pull request')
    commit_range = "--first-parent -m %s" % os.environ['TRAVIS_COMMIT_RANGE']

    # Handle 1
    if commit_range == "":
        commit_range = "--first-parent -m -1 %s" % os.environ['TRAVIS_COMMIT']

log.debug("Using commit range `%s`", commit_range)
log.debug("Running `git log --name-only --pretty=\"format:\" %s`" % commit_range)
changes = clean_output(subprocess.check_output("git log --name-only --pretty=\"format:\" %s" % commit_range, shell=True))

# TODO:
# 1. If all tests need to be run, we're done. Search for that occurrence first
# Satisfies this requirement:
#   Anything in the toolset/ that isn't in the setup/linux/*/ subdirectory
#   Any commit message that contains [ci run]

if re.search(r'^toolset/(?!setup/linux/.+/)', changes, re.M) is not None:
    log.info("Found changes to core toolset. Running all tests.")
    print 'Run All Tests'

# 2. Changes to fw dependencies
# Satisfies this requirement:
#   ^toolset/setup/linux/.+/(.+)\.sh

# Determine what has been changed based on initial diffing output
fw_depends_changes = get_fw_depends_changes(changes)
fw_changes = get_fw_changes(changes)

# TODO: For each of these, find the files that depend on them, if we find more fw_depends stuff,
# add it to the bottom of the list, if it isn't already there.

i = 0
while True:
    if i > len(fw_depends_changes) - 1:
        break
    # Generates output of files that contain the fw_depends for this dependency
    more_changes = subprocess.check_output(['bash', '-c', 'grep -RP "fw_depends(\W|.+\W)+' + fw_depends_changes[i] + '(\W|$)" .'])
    print more_changes
    # See what frameworks need to be added
    fw_changes.extend(Set(get_fw_changes(more_changes)) - Set(fw_changes))
    # Preserves the order of the list, so we can continue with this loop
    fw_depends_changes.extend(Set(get_fw_depends_changes(more_changes)) - Set(fw_depends_changes))
    i += 1
