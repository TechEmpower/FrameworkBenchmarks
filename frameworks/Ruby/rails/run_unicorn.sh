#!/bin/bash

fw_depends rvm ruby-2.1.2 nginx

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm 2.1.2 do bundle install --gemfile=$TROOT/Gemfile

nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm 2.1.2 do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb &
