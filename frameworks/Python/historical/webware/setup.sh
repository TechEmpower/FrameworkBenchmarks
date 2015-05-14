#!/bin/bash

fw_depends python2 apache

cd webware 
rm -fr Webware Webware-1.1.1 Webware-1.1.1.tar.gz

wget downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
tar -xf Webware-1.1.1.tar.gz
cp -r app/ Webware-1.1.1/

cd $TROOT/webware/Webware-1.1.1
$PY2 install.py --no-password-prompt 
cd $TROOT/webware/Webware-1.1.1/app
$PY2 Launch.py &
