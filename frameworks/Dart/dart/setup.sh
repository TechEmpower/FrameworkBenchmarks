#!/bin/bash

sed -i 's|host: .*|host: '"${DBHOST}"'|g' postgresql.yaml

export DART_HOME=$IROOT/dart-sdk

export PUB_CACHE=$IROOT/.pubcache

$DART_HOME/bin/pub upgrade

$DART_HOME/bin/dart server.dart -a 0.0.0.0 -p 8080 -d ${MAX_THREADS} -i ${MAX_THREADS} &
