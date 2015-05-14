#!/bin/bash

fw_depends python2 apache

$PY2_ROOT/bin/pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

cd webware 
rm -fr Webware Webware-1.1.1 Webware-1.1.1.tar.gz

wget downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
tar -xf Webware-1.1.1.tar.gz
cp -r app/ Webware-1.1.1/

cd $TROOT/webware/Webware-1.1.1
python install.py --no-password-prompt 
cd $TROOT/webware/Webware-1.1.1/app
python Launch.py &
