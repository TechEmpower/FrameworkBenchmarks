#!/bin/bash

# Use to generate a new .travis.yml that runs individual
# tests within a framework directory in a new vm

# Backup original travis file
mv $FWROOT/.travis.yml $FWROOT/.travis.bak

# generate new matrix
MATRIX=`$FWROOT/toolset/run-tests.py --list-tests | sed '/FWROOT */d' | sed '/Time */d' | sed '/Results */d' | sed -E 's/(.+)/    - "TEST=\1"/g'`

tee $FWROOT/.travis.yml <<EOF
sudo: required
dist: trusty
language: generic
python:
  - "2.7"
  
env:
  matrix:
$MATRIX

before_script:
  - source ./toolset/travis/travis_clean.sh
  - source ./toolset/travis/travis_setup.sh

script:
  - tfb --mode verify --test "\$TEST"
  
cache:
  directories:
    - $HOME/.m2/repository
    - $HOME/.cache/pip
EOF

