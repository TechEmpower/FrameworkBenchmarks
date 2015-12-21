#!/bin/bash

#fw_depends nginx xsp mono
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

sed -i 's|localhost|'"${DBHOST}"'|g' src/Web.config
sed -i 's|include /usr/local/nginx/conf/fastcgi_params;|include '"${NGINX_HOME}"'/conf/fastcgi_params;|g' nginx.conf

# build
rm -rf src/bin src/obj
xbuild src/NancyBenchmark.csproj /t:Clean
xbuild src/NancyBenchmark.csproj /p:Configuration=Release

echo "TROOT" + $TROOT

# Define NGINX_HOME (I don't know what happened)
NGINX_HOME="/home/blee/FrameworkBenchmarks/installs/nginx"

# nginx
port_start=9001
port_end=$((${port_start}+${MAX_THREADS}))
conf="upstream mono {\n"
for port in $(seq ${port_start} $port_end);
do
  conf+="\tserver 127.0.0.1:${port};\n"
done
conf+="}"

echo -e $conf > $TROOT/nginx.upstream.conf 
echo "씨발"
#${NGINX_HOME}/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes '"${MAX_THREADS}"';"
#${NGINX_HOME}/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes "${MAX_THREADS}";"
${NGINX_HOME}/sbin/nginx -c $TROOT/nginx.conf 

###Start fastcgi for each thread
###To debug, use --printlog --verbose --loglevels=All



# for port in $(seq ${port_start} $port_end); do
#   MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:$TROOT/src --socket=tcp:127.0.0.1:$port &
# done

#curl "http://192.168.56.102:8080/plaintext"
#curl "http://127.0.0.1:9001/plaintext"

