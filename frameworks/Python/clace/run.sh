#!/bin/sh
cd /root

cat <<EOF > /root/clhome/clace.toml
[logging]
console = false
file = false
access_logging = false
level = "WARN"

[http]
host = ""
port = 8080

[system]
enable_compression = false

[app_config]
cors.allow_origin = ""
EOF


clace server start &
sleep 2
clace app create --auth=none --approve /clace /
tail -f /dev/null
