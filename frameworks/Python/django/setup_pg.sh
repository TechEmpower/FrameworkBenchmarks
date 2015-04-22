#!/bin/bash

fw_depends python2

$PY2_ROOT/bin/pip install --install-option="--prefix=${IROOT}/py2" -r $TROOT/requirements.txt

gunicorn --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql_psycopg2 &
