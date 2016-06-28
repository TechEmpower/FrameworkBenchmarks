#!/bin/bash

fw_depends rvm ruby-2.1 nginx

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=vendor/bundle

nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb &
