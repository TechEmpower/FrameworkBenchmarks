#!/bin/bash

fw_depends mysql python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql &
