#!/bin/bash

export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python

cd $TROOT/webware/Webware-1.1.1
sudo $PY2 install.py --no-password-prompt
cd $TROOT/webware/Webware-1.1.1/app
sudo $PY2 Launch.py &
