#!/bin/bash
export NGINX_HOME=${IROOT}/nginx

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm 2.1.2 do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb &