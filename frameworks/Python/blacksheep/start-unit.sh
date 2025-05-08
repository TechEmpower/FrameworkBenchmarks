#!/usr/bin/env bash

unitd --no-daemon --control unix:/var/run/control.unit.sock &

# wait UNIT started
sleep 1

# PUT configure
curl -X PUT \
     --data-binary @unit-config.json \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config

# Then keep the container alive
wait