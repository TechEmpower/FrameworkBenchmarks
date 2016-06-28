#!/bin/bash

fw_depends rvm rbx-2.5

rvm rbx-$RBX_VERSION do \
  bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

MAX_THREADS=256 ; export MAX_THREADS
MIN_THREADS=$(( MAX_THREADS / 8 * 2 ))

rvm rbx-$RBX_VERSION do \
  bundle exec puma -t $MIN_THREADS:$MAX_THREADS -b tcp://0.0.0.0:8080 -e production &
