#!/bin/bash

fw_depends rvm ruby-2.0.0

rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile

rvm ruby-2.0.0-p0 do bundle --jobs 4

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec puma -C config/puma.rb -w 8 --preload &
