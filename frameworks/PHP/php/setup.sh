#!/bin/bash

fw_depends php nginx composer

sed -i "s|localhost|${DBHOST}|g" dborm.php
sed -i "s|localhost|${DBHOST}|g" dbraw.php
sed -i "s|localhost|${DBHOST}|g" updateraw.php
sed -i "s|localhost|${DBHOST}|g" fortune.php

sed -i "s|TEST_ROOT|${TROOT}|g" deploy/php
sed -i "s|TEST_ROOT|${TROOT}|g" deploy/nginx.conf

sed -i "s|/usr/local/nginx/|${IROOT}/nginx/|g" deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
