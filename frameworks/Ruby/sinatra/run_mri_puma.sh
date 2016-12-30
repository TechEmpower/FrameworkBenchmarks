#!/bin/bash

fw_depends mysql rvm ruby-2.0

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

WEB_SERVER=Puma DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec puma -C config/puma.rb -w 8 --preload &
