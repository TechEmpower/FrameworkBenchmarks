#!/bin/bash

fw_depends stack

sed -i 's|PG.connectHost     = "localhost"|PG.connectHost     = "'"${DBHOST}"'"|g' src/Main.hs

${IROOT}/stack --allow-different-user setup
${IROOT}/stack --allow-different-user build

${IROOT}/stack --allow-different-user exec spock-exe -- +RTS -A32m -N${MAX_THREADS} &
