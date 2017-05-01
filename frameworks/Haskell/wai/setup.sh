#!/bin/bash

fw_depends stack

cd bench

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec bench -- ${CPU_COUNT} ${DBHOST} +RTS -A32m -N${CPU_COUNT} &
