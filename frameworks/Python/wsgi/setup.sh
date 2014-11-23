#!/bin/bash

$PY2_GUNICORN hello:app -c gunicorn_conf.py &