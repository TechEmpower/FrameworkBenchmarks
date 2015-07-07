#!/bin/bash

fw_depends rvm java7 jruby-1.7.8

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml

rvm jruby-1.7.8 do bundle install --gemfile=$TROOT/Gemfile

rvm jruby-1.7.8 do bundle exec trinidad --config config/trinidad.yml &
