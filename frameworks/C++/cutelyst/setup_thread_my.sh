#!/bin/bash

DRIVER=QMYSQL
UWSGI=
NGINX=
PROCESS_OR_THREAD=-t
BALANCER=--experimental-thread-balancer

source ${TROOT}/config.sh
