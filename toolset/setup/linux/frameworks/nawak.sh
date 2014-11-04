#!/bin/bash

RETCODE=$(fw_exists nawak)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/idlewan/nawak.git nawak
cd nawak
# version 0.3.0 (2014-11-02)
git checkout b34b0b5077541ae9671957452a70e2578894d3a8

nimble update
nimble install
