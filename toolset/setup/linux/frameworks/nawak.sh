#!/bin/bash

RETCODE=$(fw_exists nawak)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/idlewan/nawak.git nawak/nawak