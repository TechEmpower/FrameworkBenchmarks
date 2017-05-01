#!/bin/bash

fw_depends mysql mongodb stack

cd yesod-mysql-mongo

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec yesod-mysql-mongo -- ${CPU_COUNT} ${DBHOST} +RTS -A32m -N${CPU_COUNT} &
