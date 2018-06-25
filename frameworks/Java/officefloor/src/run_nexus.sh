#!/bin/bash

# Obtain the script directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Write file to indicate nexus available
touch ${DIR}/nexus.available

# Run nexus for local repository for testing
docker run -p 8081:8081 --network tfb --name nexus sonatype/nexus:oss

# Clear file to indicate nexus no longer available
rm ${DIR}/nexus.available