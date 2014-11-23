#!/bin/bash

$PY2_GUNICORN wsgi:app -c gunicorn_conf.py &