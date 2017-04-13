#!/bin/bash

DRIVER=QMYSQL
UWSGI=
NGINX=
PROCESS_OR_THREAD=-t
BALANCER=--experimental-thread-balancer
export CUTELYST_EVENT_LOOP_EPOLL=1

source ${TROOT}/config.sh
