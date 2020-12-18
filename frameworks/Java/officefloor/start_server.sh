#!/bin/bash

# Determine memory to use
MEMORY_LINE=`free --mega | grep 'Mem'`
FREE=`echo $MEMORY_LINE | awk '{print $4}'`
AVAILABLE=`echo $MEMORY_LINE | awk '{print $7}'`
USABLE=$(($FREE + $AVAILABLE))

# Ensure able to determine available
if [[ -z $USABLE ]]; then
	USABLE=$FREE
fi

# Determine remaining after heap and process memory
BUFFER_USABLE=$(($USABLE - 4096))

# Indicate memory
echo "FREE $FREE , AVAILABLE $AVAILABLE , USABLE $USABLE , BUFFER_USABLE $BUFFER_USABLE"

# Determine max direct memory command
if (($BUFFER_USABLE > 6144)); then
  # Have memory to increase buffer size
  MAX_DIRECT=-XX:MaxDirectMemorySize=6g
else
  # No additional memory available, use JVM defaults
  MAX_DIRECT=""
fi

# Indicate memory
echo "Using direct memory option $MAX_DIRECT"

# Start server
java -server -Xms2g -Xmx2g -XX:+UseNUMA $MAX_DIRECT -Dofficefloor.socket.memory.threshold.percentage=0.1 -Dofficefloor.socket.max.reads.on.select=1 -jar server.jar

