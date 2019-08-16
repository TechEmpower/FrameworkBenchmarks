#!/bin/bash

CPU_COUNT=$(nproc)

# one fastcgi instance for each thread
# load balanced by nginx
socket_index_start=1
socket_index_end=$(($socket_index_start+$CPU_COUNT))

# To debug, use --printlog --verbose --loglevels=All
for port in $(seq $socket_index_start $socket_index_end); do
	MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:/aspnet/src --filename=/tmp/fastcgi-mono-server4-$port --socket=unix &
done

sleep 5s

# nginx
conf="upstream mono {\n"
for port in $(seq $socket_index_start $socket_index_end); do
  chmod -R 777 /tmp/fastcgi-mono-server4-$port
  conf+="\tserver unix:/tmp/fastcgi-mono-server4-${port};\n"
done
conf+="}"

echo -e $conf > nginx.upstream.conf
nginx -c /aspnet/nginx.conf

wait
