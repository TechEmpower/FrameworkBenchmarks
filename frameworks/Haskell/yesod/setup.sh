#!/bin/bash

fw_depends stack

cd bench

${IROOT}/stack --allow-different-user setup
${IROOT}/stack --allow-different-user build

${IROOT}/stack --allow-different-user exec bench ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
