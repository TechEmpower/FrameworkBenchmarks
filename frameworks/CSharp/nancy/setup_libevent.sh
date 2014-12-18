#!/bin/bash

sed -i 's|localhost|'"$DBHOST"'|g' src/Web.config

export PATH="$MONO_ROOT/bin:$PATH"

# Needed to find Mono's shared libraries
export LD_LIBRARY_PATH="$MONO_ROOT/lib"
export MONO_GC_PARAMS="nursery-size=16m"

# build mono
cd src
rm -rf bin obj
$MONO_ROOT/bin/xbuild /p:Configuration=Release

#build LibeventHost
cd $LIBEVENTHOST_HOME
xbuild /p:Configuration=Release
cd $TROOT

# nginx
conf="upstream mono {\n"
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  conf+="\tserver 127.0.0.1:"${current}";\n"
  let current=current+1
done
conf+="}"
echo -e $conf > $TROOT/nginx.upstream.conf

$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf.libevent -g "worker_processes '"${MAX_THREADS}"';"

# Start fastcgi for each thread
# To debug, use --printlog --verbose --loglevels=All
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  $MONO_ROOT/bin/mono-sgen -O=all LibeventHost/bin/Release/LibeventHost.exe 127.0.0.1 $current $DBHOST &
  let current=current+1
done