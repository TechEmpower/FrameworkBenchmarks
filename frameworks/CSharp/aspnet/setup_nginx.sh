#!/bin/bash

set -e

export NGINX_HOME=${IROOT}/nginx
export MONO_HOME=/opt/mono-20141222114925
export PATH=$MONO_HOME/bin:$PATH

sed -i 's|localhost|'"$DBHOST"'|g' src/Web.config

# extra cleaning
sudo rm -rf src/bin src/obj /tmp/nuget

xbuild src/Benchmarks.build.proj /t:Clean
xbuild src/Benchmarks.build.proj /t:Build

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

$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes ${MAX_THREADS};"

# Start fastcgi for each thread
# To debug, use --printlog --verbose --loglevels=All
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  fastcgi-mono-server4 --applications=/:${TROOT}/src --socket=tcp:127.0.0.1:$current &
  let current=current+1
done
