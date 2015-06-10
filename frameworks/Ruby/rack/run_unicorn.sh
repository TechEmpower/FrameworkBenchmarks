#!/bin/bash
source $IROOT/java7.installed
export NGINX_HOME=${IROOT}/nginx

sed -i 's|  host:.*|  host:'"${DBHOST}"'|g' config/database.yml
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

$NGINX_HOME/sbin/nginx -c $TROOT/config/nginx.conf

rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb &