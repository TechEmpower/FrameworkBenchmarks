#!/bin/bash

fw_depends rvm jruby-1.7

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml

rvm jruby-$JRUBY_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

rvm jruby-$JRUBY_VERSION do bundle exec torqbox -b 0.0.0.0 -E production &
