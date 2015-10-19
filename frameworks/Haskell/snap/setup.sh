#!/bin/bash

sed -i 's|host=".*"|host="'"${DBHOST}"'"|g' bench/cfg/db.cfg

fw_depends haskell

cd bench

cabal update
cabal sandbox init
cabal --bindir=${TROOT}/bench/dist/build/snap-bench install

dist/build/snap-bench/snap-bench +RTS -A4M -N -qg2 -I0 -G2 &
