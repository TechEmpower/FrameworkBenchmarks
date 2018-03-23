#!/bin/bash

service php5.6-fpm start
nginx -c /cakephp/deploy/nginx.conf
sleep infinity
