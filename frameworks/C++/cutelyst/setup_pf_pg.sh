#!/bin/bash

DRIVER=QPSQL
UWSGI=
NGINX=
C_PROCESSES=$(( (${CPU_COUNT}+1) / 2 ))
C_THREADS=0
CPU_AFFINITY=2

source ${TROOT}/config.sh
