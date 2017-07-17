#!/bin/bash

fw_depends mysql rvm nginx ruby-2.2

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/padrino/bundle

nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-$MRI_VERSION do bundle exec unicorn -E production -c config/unicorn.rb &
