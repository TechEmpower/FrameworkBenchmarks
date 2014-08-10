#!/bin/bash
#
# Our travis setup relies heavily on rapidly cancelling 
# jobs that are unneeded. For example, if only files
# in aspnet/ were modified, then we don't need to run 
# a job that verifies the framework tests in go/
#
# Detecting what files have been changed for a pull 
# request is a a bit tricky, as Travis-CI currently 
# does not provide a commit range for pull requests
# (see travis-ci/travis-ci#1719). This script provides
# a commit range by adding a branch prbase on the 
# first commit in the pull request and a branch 
# prhead on the commit that travis is currently 
# building. 

# This makes 
#     git diff prbase:prhead 
# equivalent to what you'd expect from 
#     git diff $TRAVIS_COMMIT_RANGE

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]
  then 
  echo "This is not a pull request, nothing to do"
  exit 0
fi

# Find the first commit in the pull request
#   - download github PR patch file
#   - grep commit SHA from first line
#   - remove newline
PR_FIRST=$(curl -s https://github.com/${TRAVIS_REPO_SLUG}/pull/${TRAVIS_PULL_REQUEST}.patch | head -1 | grep -o -E '\b[0-9a-f]{40}\b' | tr -d '\n')

# Use the parent of the first commit so we can 
# easily diff the changes introduced by this PR
PR_PARENT=$(git rev-parse ${PR_FIRST}^)

# Create new branch for first commit in pull request
git branch -f prbase ${PR_PARENT}

# Create new branch for last commit in pull request
# Note: This is the automerge commit that github creates
#       If you really want the last commit use the 
#       parent of this commit. It's a merge, so you have to 
#       decipher which parent commit you want
git branch -f prhead ${TRAVIS_COMMIT}

echo "Set prbase branch to commit ${PR_PARENT}"
echo "Set prhead branch to commit ${TRAVIS_COMMIT}"
