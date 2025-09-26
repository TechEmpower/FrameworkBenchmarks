#!/usr/bin/env bash

config='{'
config+='  "listeners": {'
config+='    "*:8080": {'
config+='      "pass": "applications/example"'
config+='    }'
config+='  },'
config+='  "applications": {'
config+='    "example": {'
config+='      "type": "external",'
config+='      "processes": '"$(nproc)"','
config+='      "executable": "/app/example"'
config+='    }'
config+='  }'
config+='}'

curl -X PUT \
     --data-binary "$config" \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config
