#!/bin/bash

rm -rf ${LWAN_BUILD}
mkdir -p ${LWAN_BUILD}
cd ${LWAN_BUILD}
cmake ${LWAN_ROOT} -DCMAKE_BUILD_TYPE=Release && make techempower
cd $LWAN_ROOT/techempower
$LWAN_BUILD/techempower/techempower &
