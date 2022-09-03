#!/usr/bin/env bash

processes=$(expr $(nproc) / 2)

config='{'
config+='  "listeners": {'
config+='    "*:8080": {'
config+='      "pass": "applications/example"'
config+='    }'
config+='  },'
config+='  "applications": {'
config+='    "example": {'
config+='      "type": "external",'
config+='      "processes": '"$processes"','
config+='      "executable": "/app/example",'
config+='      "environment": {'
config+='        "SCALANATIVE_GC_THREADS": "2"'
config+='      }'
config+='    }'
config+='  }'
config+='}'

curl -X PUT \
     --data-binary "$config" \
     --unix-socket /var/run/control.unit.sock \
     http://localhost/config
