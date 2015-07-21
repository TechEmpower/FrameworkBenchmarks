#!/bin/bash
export CABAL_HOME=/opt/cabal/1.20
export HASKELL_HOME=/opt/ghc/7.8.3
export LANG=en_US.UTF-8

sed -i 's|host=".*"|host="'"${DBHOST}"'"|g' bench/cfg/db.cfg

export PATH=${HASKELL_HOME}/bin:$PATH

cd bench

${CABAL_HOME}/bin/cabal update
${CABAL_HOME}/bin/cabal sandbox init
${CABAL_HOME}/bin/cabal --bindir=${TROOT}/bench/dist/build/snap-bench install

dist/build/snap-bench/snap-bench +RTS -A4M -N -qg2 -I0 -G2 &