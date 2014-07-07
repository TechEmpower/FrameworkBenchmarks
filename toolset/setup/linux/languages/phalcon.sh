#!/bin/bash

RETCODE=$(fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/phalcon.so)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends php

test -d cphalcon || git clone git://github.com/phalcon/cphalcon.git
cd cphalcon/build && sudo ./install