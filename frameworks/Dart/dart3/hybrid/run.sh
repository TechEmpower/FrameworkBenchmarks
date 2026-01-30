#!/bin/sh
# Detect CPU threads from the system
TOTAL_CORES=$(grep -c ^processor /proc/cpuinfo)

# This comes from the ENV in the Dockerfile
MAX_ISO=${MAX_ISOLATES_PER_PROCESS}

# Calculate OS processes needed
NUM_WORKERS=$((TOTAL_CORES / MAX_ISO))
if [ "$NUM_WORKERS" -le 0 ]; then NUM_WORKERS=1; fi

# Bootstrap the calculated number of independent Dart processes.
for i in $(seq 1 $NUM_WORKERS); do
  /app/server & 
done

# Keep the shell alive to manage the backgrounded process group.
wait