#!/bin/bash

sed -i 's|/usr/local/installs/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm 2.1.2 do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb &