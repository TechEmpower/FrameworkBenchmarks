#!/bin/bash

fw_depends python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

rm -fr web2py
git clone --recursive --branch master https://github.com/web2py/web2py.git
cd web2py
git checkout 326335c3cd027a97b9c2b83d880b2b1f8f62321d
cd ..
cp -r app/standard/ web2py/applications/
cp -r app/optimized/ web2py/applications/
cp app/wsgi.py web2py/
cp app/routes.py web2py/
touch web2py/__init__.py
python compile_apps.py

gunicorn web2py.wsgi:application -c gunicorn_conf.py &
