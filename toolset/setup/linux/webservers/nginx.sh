#!/bin/bash

RETCODE=$(fw_exists /usr/local/nginx/sbin/nginx)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nginx.org/download/nginx-1.4.1.tar.gz
fw_untar nginx-1.4.1.tar.gz
cd nginx-1.4.1
./configure
make
sudo make install
