#!/bin/bash

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

rvm ruby-2.0.0-p0 do bundle --jobs 4

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &