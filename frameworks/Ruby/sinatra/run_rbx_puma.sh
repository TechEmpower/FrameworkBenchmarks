#!/bin/bash

fw_depends rvm rbx-2.4

rvm rbx-$RBX_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

WEB_SERVER=Puma DB_HOST=${DBHOST} rvm rbx-$RBX_VERSION do bundle exec puma &
