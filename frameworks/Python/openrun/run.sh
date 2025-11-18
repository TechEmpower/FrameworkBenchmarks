#!/bin/sh
cd /root

cat <<EOF > /root/openrun/openrun.toml
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


openrun server start &
sleep 2
openrun app create --auth=none --approve /openrun /
tail -f /dev/null
