#!/bin/bash

fw_depends haskell

cd bench

cabal update
cabal sandbox init
cabal --bindir=${TROOT}/bench/dist/build/bench install

dist/build/bench/bench ${MAX_THREADS} ${DBHOST} +RTS -A32m -N${MAX_THREADS} &
