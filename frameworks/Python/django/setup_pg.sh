#!/bin/bash

$PY2_GUNICORN --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql_psycopg2 &