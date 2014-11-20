#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nawak.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends nimrod

git clone git://github.com/idlewan/nawak.git nawak
cd nawak
# version 0.3.0 (2014-11-02)
git checkout b34b0b5077541ae9671957452a70e2578894d3a8

${IROOT}/nimrod/bin/nimble update
${IROOT}/nimrod/bin/nimble install

touch ${IROOT}/nawak.installed