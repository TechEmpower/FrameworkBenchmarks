#!/bin/sh
# Detect CPU threads from the system
TOTAL_CORES=$(grep -c ^processor /proc/cpuinfo)

# This comes from the ENV in the Dockerfile
MAX_ISO=${MAX_ISOLATES_PER_PROCESS}

# Calculate OS processes needed
NUM_WORKERS=$((TOTAL_CORES / MAX_ISO))
if [ "$NUM_WORKERS" -le 0 ]; then NUM_WORKERS=1; fi

echo "Scaling: $TOTAL_CORES cores / $MAX_ISO isolates = $NUM_WORKERS processes"

for i in $(seq 1 $NUM_WORKERS); do
  /bin/server & 
done

wait