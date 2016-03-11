#!/bin/bash

fw_depends stack

${IROOT}/stack --allow-different-user setup
${IROOT}/stack --allow-different-user build

${IROOT}/stack --allow-different-user exec spock-exe -- +RTS -A32m -N${MAX_THREADS} &
