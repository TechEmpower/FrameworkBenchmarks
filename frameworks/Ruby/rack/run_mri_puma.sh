#!/bin/bash

fw_depends mysql rvm ruby-2.4

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/rack/bundle

rvm ruby-$MRI_VERSION do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production &
