#!/bin/bash

fw_depends rvm nginx

# We assume single-user installation as 
# done in our rvm.sh script and 
# in Travis-CI
if [ "$TRAVIS" = "true" ]
then
  rvmsudo rvm install ruby-2.0
else
  rvm install ruby-2.0
fi

sed -i 's|CWD|'"${TROOT}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

rvm use ruby-2.0

RETCODE=$(fw_exists ${IROOT}/nginx_mruby.installed)
[ "$RETCODE" == 0 ] || { \
  git clone git://github.com/matsumoto-r/ngx_mruby.git
  cd ngx_mruby
  git submodule init
  git submodule update
  [ -d mruby/mrbgems/mruby-mysql ] || git clone git://github.com/mattn/mruby-mysql.git mruby/mrbgems/mruby-mysql

  NGINX_CONFIG_OPT_ENV="--prefix=${IROOT}/nginx_mruby --with-http_stub_status_module" sh build.sh
  make install

  echo -e "export PATH=${IROOT}/nginx_mruby/sbin:\$PATH" > $IROOT/nginx_mruby.installed
}

source $IROOT/nginx_mruby.installed

nginx -c $TROOT/nginx.conf &
