#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/cassandra.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/cassandra.installed
  return 0; }

# send over the required files
scp $FWROOT/config/cassandra/cassandra.yaml $DBHOST:~/
scp $FWROOT/config/create.js $DBHOST:~/

# install mysql on database machine
ssh $DBHOST -t "CASS_V=2.0.12
curl -Os http://archive.apache.org/dist/cassandra/$CASS_V/apache-cassandra-\$CASS_V-bin.tar.gz
sudo tar xzf apache-cassandra-\$CASS_V-bin.tar.gz -C /opt
sudo ln -s /opt/apache-cassandra-\$CASS_V /opt/cassandra

rm -rf /ssd/cassandra /ssd/log/cassandra
mkdir -p /ssd/cassandra /ssd/log/cassandra
sudo chown -R cassandra:cassandra /ssd/cassandra /ssd/log/cassandra

cp cassandra.yaml cassandra/cassandra.yaml.mod
cat <<EOF > cassandra/cass_conf_replace.sed
s/- seeds: '\([^\"]*\)'/- seeds: '$DBHOST'/
s/listen_address: \(.*\)/listen_address: $DBHOST/
s/rpc_address: \(.*\)/rpc_address: $DBHOST/
EOF
sed -i -f cassandra/cass_conf_replace.sed cassandra/cassandra.yaml.mod

sudo cp -f cassandra/cassandra.init /etc/init.d/cassandra
sudo cp -f cassandra/cassandra.init.env /etc/default/cassandra
sudo cp -f cassandra/cassandra.yaml.mod /opt/apache-cassandra-\$CASS_V/conf/cassandra.yaml
sudo cp -f cassandra/log4j-server.properties /opt/apache-cassandra-\$CASS_V/conf

sudo update-rc.d cassandra defaults
sudo service cassandra restart"

echo -e "ssh \$DBHOST -t 'mongo < create.js'" > $IROOT/cassandra.installed

source $IROOT/cassandra.installed
