#!/bin/bash

fw_depends python2

gunicorn hello:app -c gunicorn_conf.py &
