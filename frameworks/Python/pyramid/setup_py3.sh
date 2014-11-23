#!/bin/bash

$PY3_GUNICORN wsgi:app -c gunicorn_conf.py &