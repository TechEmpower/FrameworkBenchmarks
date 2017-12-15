#!/bin/bash

fw_depends mysql php7 nginx

sed -i 's|localhost|'"${DBHOST}"'|g' deploy/nginx.conf
sed -i 's|root .*/FrameworkBenchmarks/php-symfony| root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
php -r "if (hash_file('SHA384', 'composer-setup.php') === '544e09ee996cdf60ece3804abc52599c22b1f40f4323403c44d44fdfdd586475ca9813a858088ffbc1f233e9b180f061') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
php composer-setup.php
php -r "unlink('composer-setup.php');"

export APP_ENV=prod

php composer.phar install --no-dev --no-interaction --no-progress --optimize-autoloader --classmap-authoritative

php bin/console cache:clear --env=prod --no-debug --no-warmup
php bin/console cache:warmup --env=prod --no-debug

php-fpm --fpm-config $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
