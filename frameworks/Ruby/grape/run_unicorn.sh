#!/bin/bash

fw_depends rvm nginx ruby-2.0.0

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

nginx -c $TROOT/config/nginx.conf

rvm ruby-2.0.0-p0 do bundle install --gemfile=$TROOT/Gemfile --path vendor/bundle

rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &
