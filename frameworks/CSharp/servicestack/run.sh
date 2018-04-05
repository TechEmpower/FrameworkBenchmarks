#!/bin/bash

CPU_COUNT=$(nproc)

# one fastcgi instance for each thread
# load balanced by nginx
port_start=9001
port_end=$(($port_start+$CPU_COUNT))

# To debug, use --printlog --verbose --loglevels=All
for port in $(seq $port_start $port_end); do
	MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:/servicestack/src --socket=tcp:127.0.0.1:$port &
done

sleep 5s

# nginx
conf="upstream mono {\n"
for port in $(seq $port_start $port_end); do
  conf+="\tserver 127.0.0.1:${port};\n"
done
conf+="}"

echo -e $conf > nginx.upstream.conf
nginx -c /servicestack/nginx.conf -g "worker_processes ${CPU_COUNT};"

wait
