#!/bin/bash

fw_depends php
fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/phalcon.so
[ $? -ne 0 ] || { return 0; }

test -d cphalcon || git clone git://github.com/phalcon/cphalcon.git
cd cphalcon/build && sudo ./install