#!/bin/bash

fw_depends python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

rm -fr web2py
git clone --recursive --branch master https://github.com/web2py/web2py.git
cd web2py
# The following commit is checked out in order to take advantage of a particular
# bug fix that was made after the latest stable release. This can be changed
# in future rounds once a new stable release has been made.
git checkout 326335c3cd027a97b9c2b83d880b2b1f8f62321d
cd ..
cp -r app/standard/ web2py/applications/
cp -r app/optimized/ web2py/applications/
cp app/wsgi.py web2py/
cp app/routes.py web2py/
touch web2py/__init__.py
$PY2_ROOT/bin/python compile_apps.py

gunicorn web2py.wsgi:application -c gunicorn_conf.py &
