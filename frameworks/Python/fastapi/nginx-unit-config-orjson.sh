#!/usr/bin/env bash

config='{'
config+='  "listeners": {'
config+='    "*:8080": {'
config+='      "pass": "applications/fastapi"'
config+='    }'
config+='  },'
config+='  "applications": {'
config+='    "fastapi": {'
config+='      "type": "python",'
config+='      "path": "/fastapi",'
config+='      "home": "/opt/venv/",'
config+='      "protocol": "asgi",'
config+='      "module": "app_orjson",'
config+='      "callable": "app",'
config+='      "processes": '"$(nproc)"','
config+='    }'
config+='  }'
config+='}'

curl -X PUT \
     --data-binary "$config" \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config
