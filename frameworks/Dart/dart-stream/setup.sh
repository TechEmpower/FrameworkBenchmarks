#!/bin/bash
export DART_HOME=$IROOT/dart-sdk
export PUB_CACHE=$IROOT/.pubcache
export NGINX_HOME=$IROOT/nginx

sed -i 's|host: .*|host: '"${DBHOST}"'|g' postgresql.yaml
sed -i 's|host: .*|host: '"${DBHOST}"'|g' mongodb.yaml

$DART_HOME/bin/pub upgrade

#
# start dart servers
#
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  $DART_HOME/bin/dart server.dart -a 127.0.0.1 -p $current -d ${MAX_THREADS} &
  let current=current+1
done


#
# create nginx configuration
#
conf+="worker_processes ${MAX_THREADS};\n"
conf+="error_log /dev/null error;\n"
conf+="events {\n"
conf+="\tworker_connections 1024;\n"
conf+="}\n"
conf+="http {\n"
conf+="\taccess_log off;\n"
conf+="\tinclude ${NGINX_HOME}/conf/mime.types;\n"
conf+="\tdefault_type application/octet-stream;\n"
conf+="\tsendfile on;\n"
conf+="\tupstream dart_cluster {\n"
current=9001
end=$(($current+$MAX_THREADS))
while [ $current -lt $end ]; do
  conf+="\t\tserver 127.0.0.1:${current};\n"
  let current=current+1
done
conf+="\t\tkeepalive ${MAX_THREADS};\n"
conf+="\t}\n"
conf+="\tserver {\n"
conf+="\t\tlisten 8080;\n"
conf+="\t\tlocation / {\n"
conf+="\t\t\tproxy_pass http://dart_cluster;\n"
conf+="\t\t\tproxy_http_version 1.1;\n"
conf+="\t\t\tproxy_set_header Connection \"\";\n"
conf+="\t\t}\n"
conf+="\t}\n"
conf+="}"
#
# write nginx configuration to disk
#
echo -e $conf > nginx.conf

$NGINX_HOME/sbin/nginx -c $(pwd)/nginx.conf &