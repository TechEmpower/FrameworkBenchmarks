#!/bin/bash

PREFIX=${IROOT}/nginx_mruby

RETCODE=$(fw_exists ${IROOT}/nginx_mruby.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends rvm
rvm install ruby-2.0.0-p0
rvm use ruby-2.0.0-p0
git clone git://github.com/matsumoto-r/ngx_mruby.git
cd ngx_mruby
git submodule init
git submodule update
[ -d mruby/mrbgems/mruby-mysql ] || git clone git://github.com/mattn/mruby-mysql.git mruby/mrbgems/mruby-mysql

NGINX_CONFIG_OPT_ENV="--prefix=${PREFIX} --with-http_stub_status_module" sh build.sh
make install

touch ${IROOT}/nginx_mruby.installed
