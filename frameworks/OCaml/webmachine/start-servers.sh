#!/bin/sh

CMD=$1
CPU_COUNT=$(nproc)
P=9000
END=$(($P+$CPU_COUNT))

while [ $P -lt $END ]; do
  PORT=$P $CMD &
  P=$((P+1))
done
sleep 2
exec /usr/sbin/haproxy -f /etc/haproxy/haproxy.cfg -p /run/haproxy.pid
