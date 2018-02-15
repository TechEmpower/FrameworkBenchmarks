#!/bin/bash

fw_depends mysql php7 nginx composer

sed -i 's|localhost|'"${DBHOST}"'|g' bench/app/config/databases.php
sed -i "s|APP_ROOT|${TROOT}/bench/app/|g" bench/public/index.php
sed -i "s|CORE_ROOT|${TROOT}/vendor/Kumbia/core/|g" bench/public/index.php
sed -i "s|TEST_ROOT|${TROOT}/bench/public/|g" deploy/php
sed -i "s|TEST_ROOT|${TROOT}/bench/public/|g" deploy/nginx.conf
sed -i "s|INSTALL_ROOT|${IROOT}|g" deploy/nginx.conf


[[ ! -e vendor/Kumbia ]] || rm -rf vendor/Kumbia

git clone -b 1.0 --single-branch https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia
git clone -b dev --single-branch https://github.com/KumbiaPHP/ActiveRecord.git vendor/Kumbia/ActiveRecord

php-fpm --fpm-config $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf -g /tmp/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
