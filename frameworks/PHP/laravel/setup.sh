#!/bin/bash

fw_depends mysql php7 nginx composer

sed -i 's|__DBHOST__|'${DBHOST}'|g' .env

sed -i 's|__IROOT__|'${IROOT}'|g' deploy/nginx.conf
sed -i 's|__TROOT__|'${TROOT}'|g' deploy/nginx.conf

php artisan config:cache
php artisan route:cache

php-fpm --fpm-config ${FWROOT}/toolset/setup/linux/languages/php/php-fpm.conf -g ${TROOT}/deploy/php-fpm.pid
nginx -c ${TROOT}/deploy/nginx.conf
