#!/bin/bash

export NGINX_HOME=${IROOT}/nginx

set -e
# mono environment variables
. ${IROOT}/mono.installed
sed -i 's|localhost|'"$DBHOST"'|g' src/Web.config
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' nginx.conf

# extra cleaning
rm -rf src/bin src/obj
xbuild src/ServiceStackBenchmark.csproj /t:Clean
xbuild src/ServiceStackBenchmark.csproj /t:Build
# one fastcgi instance for each thread
# load balanced by nginx
port_start=9001
port_end=$(($port_start+$MAX_THREADS))
# nginx
conf="upstream mono {\n"
for port in $(seq $port_start $port_end); do
conf+="\tserver 127.0.0.1:${port};\n"
done
conf+="}"
echo -e $conf > $TROOT/nginx.upstream.conf
$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes ${MAX_THREADS};"
# To debug, use --printlog --verbose --loglevels=All
for port in $(seq $port_start $port_end); do
	MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:${TROOT}/src --socket=tcp:127.0.0.1:$port &
done
