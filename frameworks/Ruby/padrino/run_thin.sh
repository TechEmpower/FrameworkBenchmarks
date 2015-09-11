#!/bin/bash

fw_depends rvm ruby-2.0.0

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec thin start -C config/thin.yml &
