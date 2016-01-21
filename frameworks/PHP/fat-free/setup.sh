#!/bin/bash

fw_depends php7 nginx

sed -i 's|localhost|'"${DBHOST}"'|g' index.php
sed -i 's|root .*/FrameworkBenchmarks/fat-free|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

F3DIR="$TROOT/src"

[[ ! -e "$F3DIR" ]] || rm -r "$F3DIR"

git clone "https://github.com/bcosca/fatfree-core.git" "$F3DIR"
pushd "$F3DIR" > /dev/null
git checkout -q "b284f0c482e858f0162cab529925e7de14d90746" # v3.5.0
popd > /dev/null

php-fpm --fpm-config "$FWROOT/config/php-fpm.conf" -g "$TROOT/deploy/php-fpm.pid"
nginx -c "$TROOT/deploy/nginx.conf"