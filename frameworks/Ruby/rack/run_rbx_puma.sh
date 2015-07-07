#!/bin/bash

fw_depends rvm rbx-2.2.10

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml

rvm rbx-2.2.10 do bundle install --gemfile=$TROOT/Gemfile

rvm rbx-2.2.10 do bundle exec puma -b tcp://0.0.0.0:8080 -e production &
