#!/bin/bash

fw_depends mysql php7 nginx composer hhvm

sed -i 's|SourceRoot = .*/FrameworkBenchmarks/codeigniter|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
nginx -c $TROOT/deploy/nginx.conf
