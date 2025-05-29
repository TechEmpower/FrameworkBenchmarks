#!/usr/bin/env bash
CORE_COUNT=$(nproc)
sysctl -w net.core.somaxconn=65535
sysctl -w net.ipv4.tcp_max_syn_backlog=65535
ulimit -n 65535
taskset -c 0-$(($CORE_COUNT-1)) unitd --no-daemon --control unix:/var/run/control.unit.sock &

# unitd --no-daemon --control unix:/var/run/control.unit.sock &

# wait UNIT started
sleep 1

# PUT configure
curl -X PUT \
     --data-binary @unit-config.json \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config

# Then keep the container alive
wait