#!/bin/bash
touch /root/.pgpass
chmod 600 /root/.pgpass
echo tfb-database:5432:hello_world:benchmarkdbuser:benchmarkdbpass >> /root/.pgpass 
cat *.sql | psql -U benchmarkdbuser -h tfb-database -d hello_world
/usr/local/bin/postgrest /etc/postgrest.conf > /dev/null 2>&1