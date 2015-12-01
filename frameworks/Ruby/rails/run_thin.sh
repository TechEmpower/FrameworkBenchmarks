#!/bin/bash

fw_depends rvm ruby-2.1

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec thin start -C config/thin.yml &
