#!/bin/bash

fw_depends mysql python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements_py3.txt

gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql &
