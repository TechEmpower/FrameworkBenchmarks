#!/bin/bash
export NGINX_HOME=${IROOT}/nginx
source $IROOT/java7.installed

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
	source /home/travis/.rvm/scripts/rvm
else
	source $HOME/.rvm/scripts/rvm
fi

sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' config/nginx.conf

rvm ruby-2.0.0-p0 do bundle --jobs 4

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

DB_HOST=${DBHOST} rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &