#!/usr/bin/env bash

config='{'
config+='  "listeners": {'
config+='    "*:8080": {'
config+='      "pass": "applications/litestar"'
config+='    }'
config+='  },'
config+='  "applications": {'
config+='    "litestar": {'
config+='      "type": "python",'
config+='      "path": "/litestar",'
config+='      "home": "/opt/venv/",'
config+='      "protocol": "asgi",'
config+='      "module": "app",'
config+='      "callable": "app",'
config+='      "processes": '"$(nproc)"','
config+='    }'
config+='  }',
config+='  "access_log": "/dev/null"'
config+='}'

curl -X PUT \
     --data-binary "$config" \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config
