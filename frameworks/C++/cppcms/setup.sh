#!/bin/bash

fw_depends mysql postgresql

cp config.js.tpl config.js

fw_depends cppcms cppcms-cppdb
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${CPPCMS_HOME}/lib:${CPPDB_HOME}/lib

make


#Database
#http://cppcms.com/sql/cppdb/connstr.html
#http://cppcms.com/sql/cppdb/backendref.html
if [ "${DRIVER}" == "mysql" ]; then
    dbstring="mysql:host="$DBHOST";database=hello_world;user=benchmarkdbuser;password=benchmarkdbpass;set_charset_name=utf8;@pool_size=10"
else
    dbstring="postgresql:host="$DBHOST";dbname=hello_world;user=benchmarkdbuser;password=benchmarkdbpass;@pool_size=10"
fi
sed -i 's|\(.*\)--db--\(.*\)|\1'"$dbstring"'\2|g' config.js


#http://cppcms.com/wikipp/en/page/cppcms_1x_tut_web_server_config#Nginx
#configure Nginx
if [ -n "${NGINX}" ]; then
    fw_depends nginx
    nginx -c ${TROOT}/nginx.conf

    sed -i 's|\(.*\)--api--\(.*\)|\1'"fastcgi"'\2|g' config.js
    sed -i 's|\(.*\)--address--\(.*\)|\1"socket" : "/var/tmp/cppcms.sock"\2|g' config.js
    #for ip based connection
    #sed -i 's|\(.*\)--address--\(.*\)|\1"ip": "127.0.0.1" , "port" : 8081\2|g' config.js
else
    sed -i 's|\(.*\)--api--\(.*\)|\1'"http"'\2|g' config.js
    sed -i 's|\(.*\)--address--\(.*\)|\1"port": 8080\2|g' config.js
fi



./mycppcms -c config.js

