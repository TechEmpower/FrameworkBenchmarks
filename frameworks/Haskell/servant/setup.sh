#!/bin/bash

fw_depends postgres stack

${IROOT}/stack --allow-different-user setup
${IROOT}/stack --allow-different-user build

${IROOT}/stack --allow-different-user exec servant-exe -- ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
