#!/bin/bash
export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer
export PHP_FPM=${PHP_HOME}/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

sed -i 's|database_host: .*|database_host: '"${DBHOST}"'|g' app/config/parameters.yml
sed -i 's|SourceRoot = .*/FrameworkBenchmarks/symfony2|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf
sed -i 's|root .*/FrameworkBenchmarks/php-symfony2| root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

${PHP_HOME}/bin/php app/console cache:clear \
  --env=prod --no-debug --no-warmup
${PHP_HOME}/bin/php app/console cache:warmup \
  --env=prod --no-debug

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
