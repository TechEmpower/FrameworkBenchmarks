. /opt/yottadb/current/ydb_env_set
export ydb_routines="/mwebserver/r /data/munit/r $ydb_routines"
mupip rundown -f /tmp/yottadb/r1.30_x86_64/ydb-relinkctl-98fedd5406dbe07da4076a0607b49e01
rm -f /tmp/yottadb/r1.30_x86_64/ydb-relinkctl-98fedd5406dbe07da4076a0607b49e01
mumps -r ^%techempbenchmark
while :
do
   sleep 1
done
