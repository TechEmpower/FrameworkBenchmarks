#!/bin/bash

fw_depends stack

cd yesod-postgres

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec yesod-postgres -- ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
