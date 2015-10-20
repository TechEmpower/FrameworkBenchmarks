#!/bin/bash

fw_depends python3

sed -i 's|127.0.0.1|'${DBHOST}'|g' app.py

pip install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

python3 app.py &
