#!/usr/bin/env bash
# start-contrast-service.sh
set -e

# Create tfb network if it's not already up
if ! docker network inspect tfb >/dev/null 2>&1; then
  docker network create tfb >/dev/null
fi

# Create writeable logfile for contrast-service
if [ "$IN_AWS" = true ]; then
  echo "Creating writeable logfile on host"
  touch /var/log/contrast-service.log
  chmod 666 /var/log/contrast-service.log
  VOLUME="-v /var/log:/host/var/log"
fi

# Start contrast-service container
docker build . -f contrast-service.dockerfile -t contrast-service   
docker run --rm -d --network tfb $VOLUME --name contrast-service contrast-service
