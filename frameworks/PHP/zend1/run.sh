#!/bin/bash

service php7.2-fpm start
nginx -c /zend1/deploy/nginx.conf
sleep infinity
