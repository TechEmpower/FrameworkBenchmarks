#!/bin/bash

fw_depends rvm ruby-2.0.0 nginx

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path=vendor/bundle

rvm ruby-2.0.0-p0 do bundle --jobs 4

nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &
