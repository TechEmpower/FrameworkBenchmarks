#!/bin/bash

fw_depends mysql python2

sed -i 's|127.0.0.1|'${DBHOST}'|g' app.py

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

python app.py &
