#!/bin/bash

fw_depends rvm java7 jruby-1.7.8

rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile

DB_HOST=${DBHOST} rvm jruby-1.7.8 do bundle exec trinidad --config config/trinidad.yml &
