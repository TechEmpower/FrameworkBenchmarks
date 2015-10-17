#!/bin/bash

fw_depends python2 apache

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/webware/requirements.txt

cd webware 
rm -fr Webware Webware-1.1.1 Webware-1.1.1.tar.gz

fw_get -O https://downloads.sourceforge.net/webware/Webware-1.1.1.tar.gz
fw_untar Webware-1.1.1.tar.gz
cp -r app/ Webware-1.1.1/

cd $TROOT/webware/Webware-1.1.1
python install.py --no-password-prompt 
cd $TROOT/webware/Webware-1.1.1/app
python Launch.py &
