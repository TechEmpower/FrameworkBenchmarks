#!/bin/bash

fw_depends python3

$PY3_ROOT/bin/pip3 install --install-option="--prefix=${IROOT}/py3" -r $TROOT/requirements.txt

gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql &
