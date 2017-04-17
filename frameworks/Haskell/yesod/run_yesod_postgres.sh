#!/bin/bash

fw_depends postgresql stack

cd yesod-postgres

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec yesod-postgres -- ${CPU_COUNT} ${DBHOST} +RTS -A32m -N${CPU_COUNT} &
