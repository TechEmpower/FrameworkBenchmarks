#!/bin/sh

TOTAL_PROCESSES=$(grep -c ^processor /proc/cpuinfo)
MAX_ISO=${MAX_ISOLATES:-8}
RESERVED=${MIN_TRAEFIK_PROCESSES:-4}

if [ "$TOTAL_PROCESSES" -le "$((MAX_ISO * 2))" ]; then
    exec /bin/server
else
    DART_POOL=$((TOTAL_PROCESSES - RESERVED))
    NUM_WORKERS=$(( DART_POOL / MAX_ISO ))
    DART_PROCESSES=$(( NUM_WORKERS * MAX_ISO )) 
    TRAEFIK_PROCESSES=$(( TOTAL_PROCESSES - DART_PROCESSES ))

    export GOMAXPROCS=$TRAEFIK_PROCESSES

    URL_LIST=""
    for i in $(seq 1 $NUM_WORKERS); do
        PORT=$((9000 + i))
        URL_LIST="${URL_LIST}          - url: \"http://127.0.0.1:$PORT\"\n"
        /app/server --port=$PORT &
    done
    sed -i "s|# DART_WORKERS_PLACEHOLDER|$URL_LIST|" /etc/traefik/traefik_dynamic.yaml
    until nc -z 127.0.0.1 9001; do sleep 0.1; done
    exec traefik --configfile=/etc/traefik/traefik.yaml
fi