#!/bin/bash

fw_depends rvm ruby-2.2

rvm ruby-$MRI_VERSION do \
  bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

WORKERS=8 # enable Puma's clustered mode
MAX_THREADS=32 ; export MAX_THREADS
MIN_THREADS=$(( MAX_THREADS / WORKERS * 2 ))

rvm ruby-$MRI_VERSION do \
  bundle exec puma -w $WORKERS -t $MIN_THREADS:$MAX_THREADS -b tcp://0.0.0.0:8080 -e production &
