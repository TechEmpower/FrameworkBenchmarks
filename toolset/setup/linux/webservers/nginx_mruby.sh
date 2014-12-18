#!/bin/bash

PREFIX=${IROOT}/nginx_mruby

RETCODE=$(fw_exists ${IROOT}/nginx_mruby.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y libhiredis-dev
fw_depends rvm
rvm install ruby-2.0.0-p0
rvm use ruby-2.0.0-p0
#fw_depends nginx lua

#fw_get http://openresty.org/download/ngx_openresty-1.7.4.1.tar.gz
#fw_untar ngx_openresty-1.7.4.1.tar.gz
git clone git://github.com/matsumoto-r/ngx_mruby.git
cd ngx_mruby
git submodule init
git submodule update

# RETCODE=$(fw_exists mruby/mrbgems/mruby-mysql)
# if [ "$RETCODE" != 0 ] ; then
#   git clone git@github.com:mattn/mruby-mysql.git mruby/mrbgems/mruby-mysql
#   # cd mruby/mrbgems
#   # git clone git@github.com:mattn/mruby-mysql.git mruby/mrbgems/m
#   # cd ../..
# fi

NGINX_CONFIG_OPT_ENV="--prefix=${PREFIX} --with-http_stub_status_module" sh build.sh
sudo make install

touch ${IROOT}/nginx_mruby.installed
