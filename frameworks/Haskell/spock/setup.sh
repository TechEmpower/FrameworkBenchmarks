#!/bin/bash

fw_depends stack

${IROOT}/stack setup
${IROOT}/stack build

${IROOT}/stack exec spock-exe ${MAX_THREADS} +RTS -A32m -N${MAX_THREADS} &
