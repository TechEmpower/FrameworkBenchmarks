#!/bin/bash

fw_depends stack

sed -i 's|PG.connectHost     = "localhost"|PG.connectHost     = "'"${DBHOST}"'"|g' src/Main.hs

if [ "$TRAVIS" = "true" ]; then
    sed -i 's|PoolCfg 50 50 60|PoolCfg 20 25 60|g' src/Main.hs
fi

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec spock-exe -- +RTS -A32m -N${MAX_THREADS} &
