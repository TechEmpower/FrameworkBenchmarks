#!/bin/bash

RETCODE=$(fw_exists jester)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/dom96/jester.git jester
cd jester
git checkout 3e77ce753293b523004dfdaebd2fb195217f30f0
