#!/bin/hash

spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi

nginx -g 'daemon off;'
