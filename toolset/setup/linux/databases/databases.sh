#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/databases.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/databases.installed
  return 0; }

# Create a user-owned directory for our databases
ssh $DBHOST 'bash' <<EOF
sudo apt-get -y update
sudo apt-get -y install -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' build-essential libev-dev libpq-dev libreadline6-dev lsb-core
sudo mkdir -p /ssd
sudo mkdir -p /ssd/log
sudo chown -R $(whoami):$(whoami) /ssd

id -u benchmarkdbuser &> /dev/null || sudo useradd benchmarkdbuser -p benchmarkdbpass
EOF

echo -e "" > $IROOT/databases.installed

source $IROOT/databases.installed
