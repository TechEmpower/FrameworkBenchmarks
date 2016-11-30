#!/bin/bash

fw_depends stack

cd yesod-mysql-mongo

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec yesod-mysql-mongo -- ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
