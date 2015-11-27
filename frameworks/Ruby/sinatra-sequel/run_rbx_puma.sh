#!/bin/bash

fw_depends rvm rbx-2.2.10

rvm rbx-2.2.10 do bundle install --gemfile=$TROOT/Gemfile

DB_HOST=${DBHOST} rvm rbx-2.2.10 do bundle exec puma &
