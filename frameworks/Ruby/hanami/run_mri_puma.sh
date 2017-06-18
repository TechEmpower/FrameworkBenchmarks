#!/bin/bash

fw_depends mysql rvm ruby-2.4

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/hanami/bundle

DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &
