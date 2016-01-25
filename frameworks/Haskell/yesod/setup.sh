#!/bin/bash

fw_depends stack

cd bench

${IROOT}/stack setup
${IROOT}/stack build

${IROOT}/stack exec bench ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
