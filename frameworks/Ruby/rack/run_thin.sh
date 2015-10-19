#!/bin/bash

fw_depends rvm ruby-2.0.0

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle

rvm ruby-2.0.0-p0 do bundle exec thin start -C config/thin.yml &
