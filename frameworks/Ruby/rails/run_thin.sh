#!/bin/bash

fw_depends rvm ruby-2.1.2

rvm 2.1.2 do bundle install --gemfile=$TROOT/Gemfile

DB_HOST=${DBHOST} rvm ruby-2.1.2 do bundle exec thin start -C config/thin.yml &
