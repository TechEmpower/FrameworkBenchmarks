#!/bin/bash

RETCODE=$(fw_exists jester.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/dom96/jester.git jester
cd jester
# most recent commit in branch new-async as of 2014-10-31
git checkout 3e77ce753293b523004dfdaebd2fb195217f30f0

cd ..
touch jester.installed