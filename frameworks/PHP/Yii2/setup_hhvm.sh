#!/bin/bash

fw_depends php nginx composer hhvm

sed -i 's|localhost|'"${DBHOST}"'|g' app/index.php
sed -i 's|SourceRoot = .*/FrameworkBenchmarks/Yii2|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf
sed -i 's|root .*/FrameworkBenchmarks/php-yii2|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
nginx -c $TROOT/deploy/nginx.conf
