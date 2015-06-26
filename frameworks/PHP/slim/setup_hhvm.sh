#!/bin/bash

fw_depends php nginx hhvm

sed -i 's|localhost|'"${DBHOST}"'|g' index.php
sed -i 's|SourceRoot = .*/FrameworkBenchmarks/php-slim|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf
sed -i 's|root .*/FrameworkBenchmarks/php-slim| root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
nginx -c $TROOT/deploy/nginx.conf
