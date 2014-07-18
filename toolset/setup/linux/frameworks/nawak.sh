#!/bin/bash

RETCODE=$(fw_exists nawak)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/idlewan/nawak.git nawak/nawak
cd nawak/nawak
git checkout 5e56d718ff327c58cbdca14d44abc327f752681d
