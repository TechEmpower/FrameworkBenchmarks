#!/usr/bin/env python

# @file:        toolset/travis/travis_diff.py
# @author:      Nate Brady
# @description: This script is only for use within Travis-CI. It is meant to look through the commit history
#   and determine whether or not the current framework test directory needs to be run.
#
# Notes: This script will run in python 2 and 3. print is being used instead of the logging import because
#   travis does not echo python logging during the before_script lifecycle.

# TODO: Needs to be updated to look at the new Travis test possibilities
#       of TEST, TESTDIR, and TESTLANG. IE. Only run a single framework test
#       in TESTLANG if it's the only one that's changed

import subprocess
import os
import re
from sets import Set


# Returns a unique list of dockerfile changes
def get_docker_changes(changes_output):
    return list(
        Set(
            re.findall(r"toolset/setup/docker/.+/(.+)\.dockerfile", changes_output,
                       re.M)))


def fw_found_in_changes(changes_output):
    if os.getenv("TESTLANG"):
        return re.search(
            r"frameworks/" + re.escape(os.getenv("TESTLANG")) + "/",
            changes_output, re.M)
    elif os.getenv("TESTDIR"):
        return re.search(
            r"frameworks/" + re.escape(os.getenv("TESTDIR")) + "/",
            changes_output, re.M)
    elif os.getenv("TEST"):
        for test in os.getenv("TEST").split(" "):
            if re.search(
                  r"frameworks/.+/" + re.escape(os.getenv("TEST")) + "/",
                  changes_output, re.M):
                return True


# Cleans up diffing and grep output and into an array of strings
def clean_output(output):
    return os.linesep.join([s for s in output.splitlines() if s])


def quit_diffing(should_test_run):
    if should_test_run:
        print("travis-diff-continue")
    exit(0)


# COMMIT MESSAGES:
# Before any complicated diffing, check for forced runs from the commit message
# Use -2 because travis now inserts a merge commit as the last commit
last_commit_msg = subprocess.check_output(
    ['bash', '-c', 'git log -2 --pretty=%B'])
print("Parsing commit message for travis commands: {!s}"
      .format(last_commit_msg))

# Forced full run
if re.search(r'\[ci run-all\]', last_commit_msg, re.M):
    print("All tests have been forced to run from the commit message.")
    quit_diffing(True)

# TODO: Fix to work with TESTLANG and TEST
# Forced *fw-only* specific tests
if re.search(r'\[ci fw-only.+\]', last_commit_msg, re.M):
    if os.getenv("TESTDIR") and re.search(
            r'\[ci fw-only(.?)+ ' + re.escape(os.getenv("TESTDIR")) +
            '( .+\]|])', last_commit_msg, re.M):
        print("This test has been forced to run from the commit message.")
        quit_diffing(True)
    else:
        print("Skipping this test from the commit message.")
        quit_diffing(False)

# TODO: Fix to work with TESTLANG and TEST
# Forced framework run
if os.getenv("TESTDIR") and re.search(
        r'\[ci fw(.?)+ ' + re.escape(os.getenv("TESTDIR")) + '( .+\]|\])',
        last_commit_msg, re.M):
    print('This test has been forced to run from the commit message.')
    quit_diffing(True)

print("TRAVIS_COMMIT_RANGE: {!s}".format(os.getenv("TRAVIS_COMMIT_RANGE")))
print("TRAVIS_COMMIT      : {!s}".format(os.getenv("TRAVIS_COMMIT")))

is_PR = (os.getenv("TRAVIS_PULL_REQUEST") != "false")
commit_range = ""

if is_PR:
    print('I am testing a pull request')
    first_commit = os.getenv("TRAVIS_COMMIT_RANGE").split('...')[0]
    last_commit = subprocess.check_output(
        "git rev-list -n 1 FETCH_HEAD^2", shell=True).rstrip('\n')
    print("Guessing that first commit in PR is : {!s}".format(first_commit))
    print("Guessing that final commit in PR is : {!s}".format(last_commit))

    if first_commit == "":
        print("No first commit, using Github's automerge commit")
        commit_range = "--first-parent -1 -m FETCH_HEAD"
    elif first_commit == last_commit:
        print("Only one commit in range, examining {!s}".format(last_commit))
        commit_range = "-m --first-parent -1 {!s}".format(last_commit)
    else:
        commit_range = "--first-parent {!s}...{!s}".format(
            first_commit, last_commit)

if not is_PR:
    print('I am not testing a pull request')
    commit_range = "--first-parent -m {!s}".format(
        os.getenv("TRAVIS_COMMIT_RANGE"))

    # Handle 1
    if commit_range == "":
        commit_range = "--first-parent -m -1 {!s}".format(
            os.getenv("TRAVIS_COMMIT"))

print("Using commit range `{!s}`".format(commit_range))
print("Running `git log --name-only --pretty=\"format:\" {!s}`".format(
    commit_range))
changes = clean_output(
    subprocess.check_output([
        'bash', '-c',
        'git log --name-only --pretty="format:" {!s}'.format(commit_range)
    ]))
print("Determining what to run based on the following file changes: \n{!s}"
      .format(changes))

# TODO: any changes in the toolset folder will generate a full run.
#       Instead limit this to core toolset files and work on diffing
#       docker dependencies
# Ignore travis and docker directory changes
# Also for now, ignore the old linux setup folders, as we don't want to
# trigger a full run as we remove old fw_depends scripts. [ci run-all] will
# still work if it's needed.

if re.search(r'^toolset/(?!(travis/|setup/|continuous/))', changes, re.M) is not None:
    print("Found changes to core toolset. Running all tests.")
    quit_diffing(True)

if fw_found_in_changes(changes):
    print("Found changes that affect this framework. Running test.")
    quit_diffing(True)

# Determine what has been changed based on initial diffing output
docker_changes = get_docker_changes(changes)

# For each of these, find the files that depend on them, if we find more
# docker FROM dependencies add it to the bottom of the list, if it isn't
# already there.
i = 0
while i <= len(docker_changes) - 1:

    # Generates output of files that contain a FROM import for this dependency
    more_changes = subprocess.check_output([
        'bash', '-c', 'grep -RP "FROM tfb/' +
                      re.escape(docker_changes[i].replace('.dockerfile', ''))
                      + '(:|$)" . || echo ""'
    ])
    print("more_changes: {!s}".format(more_changes))
    if fw_found_in_changes(more_changes):
        print("Found changes that affect this framework. Running test.")
        quit_diffing(True)

    # Preserves the order of the list, so we can continue with this loop
    docker_changes.extend(
        Set(get_docker_changes(more_changes)) - Set(docker_changes))
    i += 1

# If we get here, there was nothing found
print("Did not find any changes that affect this framework.")
quit_diffing(False)
