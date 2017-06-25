#!/bin/bash

fw_depends mysql rvm ruby-2.4 nginx

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/rails/bundle

nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb &
