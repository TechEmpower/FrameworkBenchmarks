#!/bin/bash

fw_depends mysql rvm ruby-2.0

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

WEB_SERVER=Thin DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec thin start -C config/thin.yml &
