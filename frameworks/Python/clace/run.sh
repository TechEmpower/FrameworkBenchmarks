#!/bin/sh
cd /root

cat <<EOF > /root/clhome/clace.toml
[logging]
console = false
file = false
access_logging = false

[http]
host = "0.0.0.0"
port = 8080
EOF


clace server start &
sleep 2
clace app create --auth=none --approve /clace tfb-server:/
tail -f /dev/null
