#!/bin/bash
export CABAL_HOME=/opt/cabal/1.20
export HASKELL_HOME=/opt/ghc/7.8.3
export LANG=en_US.UTF-8

export PATH=${HASKELL_HOME}/bin:$PATH

cd bench

${CABAL_HOME}/bin/cabal update
${CABAL_HOME}/bin/cabal sandbox init
${CABAL_HOME}/bin/cabal --bindir=${TROOT}/bench/dist/build/bench install

dist/build/bench/bench ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &