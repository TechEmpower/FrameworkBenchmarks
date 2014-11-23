#!/bin/bash

$PYPY_GUNICORN app:app -c gunicorn_conf.py &