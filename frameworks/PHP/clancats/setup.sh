#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' index.php
sed -i 's|root /home/ubuntu/FrameworkBenchmarks|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

fw_depends php nginx composer

rm -fr clancatsapp
rm -fr CCF
git clone --branch v2.0.6 https://github.com/ClanCats/Framework.git clancatsapp

cp -r app/ clancatsapp/CCF/

cp -r CCF/vendor/ clancatsapp/CCF/

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
