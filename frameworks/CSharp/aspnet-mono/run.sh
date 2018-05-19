#!/bin/bash

CPU_COUNT=$(nproc)

# one fastcgi instance for each thread
# load balanced by nginx
port_start=9001
port_end=$(($port_start+$CPU_COUNT))

mkdir --mode=777 /aspnet-mono/sockets

# To debug, use --printlog --verbose --loglevels=All
for port in $(seq $port_start $port_end); do
	:>> /aspnet-mono/sockets/socket.${port}
	chmod 666 /aspnet-mono/sockets/socket.${port}
	MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:/aspnet-mono/src --socket=unix://666@mono/aspnet-mono/sockets/socket.${port} &
done

sleep 5s

# nginx
conf="upstream mono {\n"
for port in $(seq $port_start $port_end); do
  conf+="\tserver unix:/aspnet-mono/sockets/socket.${port};\n"
done
conf+="}"

echo -e $conf > nginx.upstream.conf
nginx -c /aspnet-mono/nginx.conf -g "worker_processes ${CPU_COUNT};"

wait
