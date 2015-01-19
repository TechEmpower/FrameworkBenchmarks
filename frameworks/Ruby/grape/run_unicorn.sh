#!/bin/bash
export NGINX_HOME=${IROOT}/nginx

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

sed -i 's|  host:.*|  host: '"${DBHOST}"'|g' config/database.yml
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &