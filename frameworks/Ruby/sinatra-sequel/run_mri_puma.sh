#!/bin/bash

MRI_VERSION=ruby-2.2.1

fw_depends rvm $MRI_VERSION

rvm $MRI_VERSION do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle

DB_HOST=${DBHOST} \
rvm $MRI_VERSION do bundle exec puma -w 8 -t 8:32 --preload -b tcp://0.0.0.0:8080 -e production &
