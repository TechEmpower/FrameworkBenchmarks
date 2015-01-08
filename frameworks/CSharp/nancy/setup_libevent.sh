#!/bin/bash
. ${IROOT}/mono.installed

sed -i 's|localhost|${DBHOST}|g' src/Web.config

rm -rf bin obj
xbuild src/NancyBenchmark.csproj /p:Configuration=Release

# nginx
conf="upstream mono {\n"
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  conf+="\tserver 127.0.0.1:${current};\n"
  let current=current+1
done
conf+="}"
echo -e $conf > $TROOT/nginx.upstream.conf

$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf.libevent -g "worker_processes '"${MAX_THREADS}"';"

export MONO_GC_PARAMS=nursery-size=16m

# Start fastcgi for each thread
# To debug, use --printlog --verbose --loglevels=All
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  mono-sgen -O=all LibeventHost/bin/Release/LibeventHost.exe 127.0.0.1 $current ${DBHOST} &
  let current=current+1
done