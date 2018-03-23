#!/bin/bash

service php5.6-fpm start
nginx -c /clancats/deploy/nginx.conf
sleep infinity
