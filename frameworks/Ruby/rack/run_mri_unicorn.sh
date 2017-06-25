#!/bin/bash

fw_depends mysql rvm nginx ruby-2.4

sed -i 's|127.0.0.1|'${DBHOST}'|g' config/database.yml
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-$MRI_VERSION do bundle install --jobs=4 --gemfile=$TROOT/Gemfile --path=$IROOT/rack/bundle

nginx -c $TROOT/config/nginx.conf

rvm ruby-$MRI_VERSION do bundle exec unicorn -E production -c config/unicorn.rb &
