#!/bin/bash

$PY3_GUNICORN app:app -c gunicorn_conf.py