#!/bin/bash

RETCODE=$(fw_exists jester)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone git://github.com/dom96/jester.git jester/jester
cd jester/jester
git checkout ad154e05dd93367ebd8fad9a5dc2016b67121763
