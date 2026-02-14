#!/bin/sh

# --- 1. Environment & Hardware Detection ---
TOTAL_PROCESSES=$(grep -c ^processor /proc/cpuinfo)
MAX_ISO=${MAX_ISOLATES:-8}
RESERVED=${MIN_TRAEFIK_PROCESSES:-4}

# --- 2. Scaling Logic ---
if [ "$TOTAL_PROCESSES" -le "$((MAX_ISO * 2))" ]; then
    exec /app/server
else
    DART_POOL=$((TOTAL_PROCESSES - RESERVED))
    NUM_WORKERS=$(( DART_POOL / MAX_ISO ))
    DART_PROCESSES=$(( NUM_WORKERS * MAX_ISO )) 
    TRAEFIK_PROCESSES=$(( TOTAL_PROCESSES - DART_PROCESSES ))

    export GOMAXPROCS=$TRAEFIK_PROCESSES

# --- 3. Generate Backend List ---
    TMP_URLS="/tmp/urls.yaml"
    true > "$TMP_URLS"

    for i in $(seq 1 $NUM_WORKERS); do
        PORT=$((9000 + i))
        echo "          - url: \"http://127.0.0.1:$PORT\"" >> "$TMP_URLS"

        /app/server --port=$PORT &
    done

# --- 4. Traefik Configuration ---
    sed -i "/# DART_WORKERS_PLACEHOLDER/ {
      r $TMP_URLS
      d
    }" /etc/traefik/traefik_dynamic.yaml
    rm "$TMP_URLS"
    
# --- 5. Readiness & Execution ---
    until nc -z 127.0.0.1 9001; do sleep 0.1; done

    exec traefik --configfile=/etc/traefik/traefik.yaml
fi