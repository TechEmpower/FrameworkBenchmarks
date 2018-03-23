#!/bin/bash

service php7.2-fpm start
nginx -c /zend/deploy/nginx.conf
sleep infinity
