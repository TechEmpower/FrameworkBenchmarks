#!/bin/bash

fw_depends rvm jruby-1.7

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml

rvm jruby-$JRUBY_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

rvm jruby-$JRUBY_VERSION do bundle exec puma -b tcp://0.0.0.0:8080 -e production &
