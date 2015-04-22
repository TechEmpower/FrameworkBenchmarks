#!/bin/bash

fw_depends python2

$PY2_ROOT/bin/pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

rm -fr web2py
git clone --recursive --branch R-2.10.3 https://github.com/web2py/web2py.git 
cp -r app/app/ web2py/applications/
cp app/routes.py web2py/

$PY2_ROOT/bin/python web2py/web2py.py -a '' -i 127.0.0.1 -p 8080  &
