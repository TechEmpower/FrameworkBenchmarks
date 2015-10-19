#!/bin/bash

fw_depends rvm ruby-2.1.2

rvm 2.1.2 do bundle install --gemfile=$TROOT/Gemfile

DB_HOST=${DBHOST} rvm ruby-2.1.2 do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &
