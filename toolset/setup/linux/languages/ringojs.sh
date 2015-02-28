#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ringojs_0.11.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get https://github.com/ringo/ringojs/releases/download/v0.11.0/ringojs_0.11_all.deb -O ringojs_0.11_all.deb
sudo apt-get install -y jsvc
sudo dpkg -i ringojs_0.11_all.deb

rm -f ringojs_0.11_all.deb

sudo ringo-admin install http://packages.ringojs.org/download/rp/latest

touch $IROOT/ringojs_0.11.installed
