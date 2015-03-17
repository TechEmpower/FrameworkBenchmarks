#!/bin/bash

$PY2_GUNICORN app:app -c gunicorn_conf.py --error-logfile log-gun.txt  &
