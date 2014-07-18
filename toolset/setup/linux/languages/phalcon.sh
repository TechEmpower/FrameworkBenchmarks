#!/bin/bash

RETCODE=$(fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/phalcon.so)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends php

test -d cphalcon || git clone git://github.com/phalcon/cphalcon.git
cd cphalcon
git checkout bf9da26e6e20ea05dd69881b9cd0c2536ec53bcb
cd build && sudo ./install
