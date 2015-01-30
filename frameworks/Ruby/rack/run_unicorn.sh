#!/bin/bash

sed -i 's|  host:.*|  host:'"${DBHOST}"'|g' config/database.yml
sed -i 's|/usr/local/installs/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
source $HOME/.rvm/scripts/rvm

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &