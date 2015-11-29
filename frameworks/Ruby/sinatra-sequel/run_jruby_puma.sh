#!/bin/bash

fw_depends rvm jruby-9k

rvm jruby-$JRUBY_VERSION do \
  bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

MAX_THREADS=256 ; export MAX_THREADS
MIN_THREADS=$(( MAX_THREADS / 8 * 2 ))

rvm jruby-$JRUBY_VERSION do \
  bundle exec puma -t $MIN_THREADS:$MAX_THREADS -b tcp://0.0.0.0:8080 -e production &
