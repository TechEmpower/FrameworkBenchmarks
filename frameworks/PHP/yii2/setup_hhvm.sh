#!/bin/bash

fw_depends mysql php7 nginx composer hhvm

sed -i 's|__TROOT__|'"${TROOT}"'|g' deploy/config.hdf

sed -i 's|root .*/FrameworkBenchmarks/php-yii2|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
nginx -c $TROOT/deploy/nginx.conf
