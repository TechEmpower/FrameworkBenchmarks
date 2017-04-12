#!/bin/bash

DRIVER=QMYSQL
UWSGI=1
NGINX=1
PROCESS_OR_THREAD=-p
CPU_AFFINITY='--cpu-affinity=1'

source ${TROOT}/config.sh
