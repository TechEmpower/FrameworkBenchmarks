#!/bin/bash

sed -i 's|host: .*|host: '"${DBHOST}"'|g' postgresql.yaml

fw_depends dart

pub upgrade

dart server.dart -a 0.0.0.0 -p 8080 -d ${MAX_CONCURRENCY} -i ${MAX_THREADS} &
