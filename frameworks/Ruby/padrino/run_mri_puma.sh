#!/bin/bash

fw_depends rvm java7 ruby-2.0.0-p0

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec puma -C config/puma.rb -w 8 --preload &
