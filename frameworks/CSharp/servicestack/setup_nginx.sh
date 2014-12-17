#!/bin/bash

sed -i 's|localhost|'"$DBHOST"'|g' src/Web.config
sed -i 's|include /usr/local/nginx/conf/fastcgi_params;|include '"${NGINX_HOME}"'/conf/fastcgi_params;|g' nginx.conf

export PATH="$MONO_ROOT/bin:$PATH"

# Needed to find Mono's shared libraries
export LD_LIBRARY_PATH="$MONO_ROOT/lib"

# build mono
cd src
rm -rf bin obj
xbuild /p:Configuration=Release

#nginx
conf="upstream mono {\n"
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  conf+="\tserver 127.0.0.1:${current};\n"
  let current=current+1
done
conf+="}"
echo -e $conf > $TROOT/nginx.upstream.conf

$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes '"${MAX_THREADS}"';"

# Start fastcgi for each thread
# To debug, use --printlog --verbose --loglevels=All
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 --applications=/:${TROOT}/src --socket=tcp:127.0.0.1:$current &
  let current=current+1
done