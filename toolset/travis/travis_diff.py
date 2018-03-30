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
            re.findall(r"toolset/setup/docker/.+/(.+)\.dockerfile",
                       changes_output, re.M)))


def fw_found_in_changes(test, changes_output):
    return re.search(r"frameworks/" + test + "/", changes_output, re.M)


# Cleans up diffing and grep output and into an array of strings
def clean_output(output):
    return os.linesep.join([s for s in output.splitlines() if s])


def quit_diffing():
    if len(run_tests):
        print("travis-run-tests {!s}".format(" ".join(set(run_tests))))
    else:
        print("No tests to run.")
    exit(0)


print("TRAVIS_COMMIT_RANGE: {!s}".format(os.getenv("TRAVIS_COMMIT_RANGE")))
print("TRAVIS_COMMIT      : {!s}".format(os.getenv("TRAVIS_COMMIT")))

is_PR = (os.getenv("TRAVIS_PULL_REQUEST") != "false")
commit_range = ""
first_commit = ""
last_commit = ""

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

# COMMIT MESSAGES:
# Before any complicated diffing, check for forced runs from the commit message
# Use -2 because travis now inserts a merge commit as the last commit
last_commit_msg = subprocess.check_output(
    ["bash", "-c", "git log --format=%B -n 1 {!s}".format(last_commit)])
print("Parsing commit message for travis commands: {!s}"
      .format(last_commit_msg))

test_dirs = []
run_tests = []

# Break the test env variable down into test directories
if os.getenv("TESTLANG"):
    test_dirs = map(lambda x: os.getenv("TESTLANG") + '/' + x,
                    os.listdir("frameworks/" + os.getenv("TESTLANG")))
elif os.getenv("TESTDIR"):
    test_dirs = os.getenv("TESTDIR").split(' ')

# Forced full run
if re.search(r'\[ci run-all\]', last_commit_msg, re.M):
    print("All tests have been forced to run from the commit message.")
    run_tests = test_dirs
    quit_diffing()

# Forced *fw-only* specific tests
if re.search(r'\[ci fw-only .+\]', last_commit_msg, re.M):
    tests = re.findall(r'\[ci fw-only (.+)\]', last_commit_msg,
                       re.M)[0].strip().split(' ')
    for test in tests:
        if test in test_dirs:
            print(
                "{!s} has been forced to run from the commit message.".format(
                    test))
            run_tests.append(test)

    # quit here because we're using "only"
    quit_diffing()

# Forced framework run in addition to other tests
if re.search(r'\[ci fw .+\]', last_commit_msg, re.M):
    tests = re.findall(r'\[ci fw (.+)\]', last_commit_msg,
                       re.M)[0].strip().split(' ')
    for test in tests:
        if test in test_dirs:
            print(
                "{!s} has been forced to run from the commit message.".format(
                    test))
            run_tests.append(test)

# TODO: any changes in the toolset folder will generate a full run.
#       Instead limit this to core toolset files and work on diffing
#       docker dependencies
# Ignore travis and docker directory changes
# Also for now, ignore the old linux setup folders, as we don't want to
# trigger a full run as we remove old fw_depends scripts. [ci run-all] will
# still work if it's needed.

if re.search(r'^toolset/(?!(travis/|setup/|continuous/))', changes,
             re.M) is not None:
    print("Found changes to core toolset. Running all tests.")
    run_tests = test_dirs
    quit_diffing()

for test in test_dirs:
    if fw_found_in_changes(test, changes):
        print("Found changes that affect {!s}".format(test))
        run_tests.append(test)
        continue

    # Determine what has been changed based on initial diffing output
    docker_changes = get_docker_changes(changes)

    # For each of these, find the files that depend on them, if we find more
    # docker FROM dependencies add it to the bottom of the list, if it isn't
    # already there.
    i = 0
    found = False
    while i <= len(docker_changes) - 1 and not found:

        # Generates output of files that contain a FROM import for this dependency
        more_changes = subprocess.check_output([
            'bash', '-c',
            'grep -RP "FROM tfb/' + re.escape(docker_changes[i].replace(
                '.dockerfile', '')) + '(:|$)" . || echo ""'
        ])
        print("more_changes: {!s}".format(more_changes))
        if fw_found_in_changes(test, more_changes):
            print("Found changes that affect {!s}".format(test))
            run_tests.append(test)
            found = True

        # Preserves the order of the list, so we can continue with this loop
        docker_changes.extend(
            Set(get_docker_changes(more_changes)) - Set(docker_changes))
        i += 1

quit_diffing()
