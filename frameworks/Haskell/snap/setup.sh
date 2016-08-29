#!/bin/bash

sed -i 's|host=".*"|host="'"${DBHOST}"'"|g' bench/cfg/db.cfg

fw_depends stack

cd bench

${IROOT}/stack --allow-different-user build --install-ghc

${IROOT}/stack --allow-different-user exec snap-bench -- +RTS -A4M -N -qg2 -I0 -G2 &
