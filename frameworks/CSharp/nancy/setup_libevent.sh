#!/bin/bash

export NGINX_HOME=${IROOT}/nginx
export LIBEVENTHOST_HOME=${TROOT}/src/LibeventHost
export MONO_GC_PARAMS="nursery-size=16m"

. ${IROOT}/mono.installed

sed -i 's|localhost|'"${DBHOST}"'|g' ${TROOT}/src/NancyBenchmark/Web.config

xbuild ${TROOT}/src/NancyBenchmark.build.proj /t:Clean
xbuild ${TROOT}/src/NancyBenchmark.build.proj /t:Rebuild /p:Configuration=Release

# nginx
port_start=9001
port_end=$((${port_start}+${MAX_THREADS}))
conf="upstream mono {\n"
for port in $(seq ${port_start} ${port_end}); do
  conf+="\tserver 127.0.0.1:${port};\n"
done
conf+="}"

echo -e $conf > ${TROOT}/nginx.upstream.conf
${NGINX_HOME}/sbin/nginx -c ${TROOT}/nginx.conf.libevent -g "worker_processes '"${MAX_THREADS}"';"

# Start fastcgi for each thread
# To debug, use --printlog --verbose --loglevels=All
for port in $(seq ${port_start} ${port_end}); do
  mono-sgen -O=all ${LIBEVENTHOST_HOME}/bin/Release/LibeventHost.exe 127.0.0.1 ${port} ${DBHOST} &
done
