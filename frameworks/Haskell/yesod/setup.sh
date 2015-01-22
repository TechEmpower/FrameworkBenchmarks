#!/bin/bash

export PATH=${HASKELL_HOME}/bin:$PATH

cd bench

${CABAL_HOME}/bin/cabal update

# Run cabal sandbox delete in order to clear old, 
# previously installed dependencies. 
${CABAL_HOME}/bin/cabal sandbox delete

${CABAL_HOME}/bin/cabal sandbox init
${CABAL_HOME}/bin/cabal --bindir=${TROOT}/bench/dist/build/bench install

dist/build/bench/bench ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &