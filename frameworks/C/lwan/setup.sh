#!/bin/bash

fw_depends lwan libjemalloc-dev libluajit-5.1-dev

cd $LWAN_ROOT/techempower
$LWAN_BUILD/techempower/techempower &
