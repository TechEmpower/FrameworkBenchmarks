#!/bin/bash

fw_depends mysql php7 nginx composer

sed -i 's|db_host|'"${DBHOST}"'|g' deploy/nginx.conf
sed -i 's|root .*/FrameworkBenchmarks/frameworks/PHP/php-symfony|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/home/vagrant/FrameworkBenchmarks/installs/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export APP_ENV=prod
php bin/console cache:clear --env=prod --no-debug --no-warmup
php bin/console cache:warmup --env=prod --no-debug

php-fpm --fpm-config ${FWROOT}/toolset/setup/linux/languages/php/php-fpm.conf -g ${TROOT}/deploy/php-fpm.pid
nginx -c ${TROOT}/deploy/nginx.conf
