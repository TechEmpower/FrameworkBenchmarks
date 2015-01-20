#!/bin/bash

sed -i 's|host=localhost;|host='"${DBHOST}"';|g' once.php.inc
sed -i 's|SourceRoot = .*/FrameworkBenchmarks/hhvm|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf

hhvm --config $TROOT/deploy/config.hdf -m daemon
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
