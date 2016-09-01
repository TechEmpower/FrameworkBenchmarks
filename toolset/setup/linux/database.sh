#!/bin/bash
#
# Configures the database server for TFB
#
# Note on Compatibility: TFB *only* supports Ubuntu 14.04 64bit
# (e.g. trusty64).

set -x
export DEBIAN_FRONTEND=noninteractive

source /etc/lsb-release
export TFB_DISTRIB_ID=$DISTRIB_ID
export TFB_DISTRIB_RELEASE=$DISTRIB_RELEASE
export TFB_DISTRIB_CODENAME=$DISTRIB_CODENAME
export TFB_DISTRIB_DESCRIPTION=$DISTRIB_DESCRIPTION

##############################
# check environment
##############################

# verify that $TFB_DBHOST is set
echo "TFB_DBHOST: $TFB_DBHOST"
[ -z "$TFB_DBHOST" ] && echo "ERROR: TFB_DBHOST is not set!"

##############################
# Prerequisites
##############################
sudo apt-get -y update
# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# Dpkg::Options avoid hangs on Travis-CI, don't affect clean systems
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    build-essential \
    git \
    libev-dev \
    libpq-dev \
    libreadline6-dev \
    postgresql        `# Installs 9.1 or 9.3, based on Ubuntu version` \
    redis-server      `# Installs 2.4 or 2.6, based on Ubuntu version` \
    lsb-core          `# Ensure that lsb_release can be used`

sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

# Create a user-owned directory for our databases
sudo mkdir -p /ssd
sudo mkdir -p /ssd/log
sudo chown -R $USER:$USER /ssd

# Additional user account (only use if required)
sudo useradd benchmarkdbuser -p benchmarkdbpass

##############################
# MySQL
##############################
echo "Setting up MySQL database"
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server

sudo stop mysql
# disable checking of disk size
sudo mv mysql /etc/init.d/mysql
sudo chmod +x /etc/init.d/mysql
sudo mv mysql.conf /etc/init/mysql.conf
# use the my.cnf file to overwrite /etc/mysql/my.cnf
sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
sudo mv my.cnf /etc/mysql/my.cnf

sudo rm -rf /ssd/mysql
sudo rm -rf /ssd/log/mysql
sudo cp -R -p /var/lib/mysql /ssd/
sudo cp -R -p /var/log/mysql /ssd/log
sudo cp usr.sbin.mysqld /etc/apparmor.d/
sudo /etc/init.d/apparmor reload
sudo start mysql

# Set root password
sudo mysqladmin -u root password secret
# Insert data
mysql -uroot -psecret < create.sql
rm create.sql

##############################
# Postgres
# Version: 9.*
##############################

echo "Setting up Postgres database"

# This will support all 9.* versions depending on the machine
PG_VERSION=`pg_config --version | grep -oP '\d\.\d'`

sudo service postgresql stop

# Sometimes this doesn't work with postgresql
sudo killall -s 9 -u postgres
sudo mv postgresql.conf /etc/postgresql/${PG_VERSION}/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/${PG_VERSION}/main/pg_hba.conf

# Make sure all the configuration files in main belong to postgres
sudo chown -Rf postgres:postgres /etc/postgresql/${PG_VERSION}/main

sudo rm -rf /ssd/postgresql
sudo cp -R -p /var/lib/postgresql/${PG_VERSION}/main /ssd/postgresql
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

sudo chown postgres:postgres /etc/sysctl.d/60-postgresql-shm.conf
sudo chown postgres:postgres create-postgres*

sudo service postgresql start

sudo -u postgres psql template1 < create-postgres-database.sql
sudo -u postgres psql hello_world < create-postgres.sql
sudo rm create-postgres-database.sql create-postgres.sql

##############################
# MongoDB
# Version: 3.2
##############################
echo "Setting up MongoDB database"
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
echo 'deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse' | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
sudo apt-get -y update
sudo apt-get -y remove mongodb-clients
sudo apt-get -y install mongodb-org

sudo service mongod stop
sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
sudo cp mongodb.conf /etc/mongodb.conf
sudo mv mongodb.conf /etc/mongod.conf
sudo rm -rf /ssd/mongodb
sudo rm -rf /ssd/log/mongodb
sudo cp -R -p /var/lib/mongodb /ssd/
sudo cp -R -p /var/log/mongodb /ssd/log/
sudo service mongod start

for i in {1..15}; do
  nc -z localhost 27017 && break || sleep 1;
  echo "Waiting for MongoDB ($i/15}"
done
nc -z localhost 27017
if [ $? -eq 0 ]; then
  mongo < create.js
  rm create.js
  mongod --version
else
  >&2 echo "MongoDB did not start, skipping"
fi

##############################
# Apache Cassandra
##############################
echo "Setting up Apache Cassandra database"
##sudo apt-get install -qqy openjdk-7-jdk
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install -qqy oracle-java8-installer
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections

echo "deb http://www.apache.org/dist/cassandra/debian 22x main" | sudo tee -a /etc/apt/sources.list.d/cassandra.sources.list
echo "deb-src http://www.apache.org/dist/cassandra/debian 22x main" | sudo tee -a /etc/apt/sources.list.d/cassandra.sources.list
gpg --keyserver pgp.mit.edu --recv-keys F758CE318D77295D
gpg --export --armor F758CE318D77295D | sudo apt-key add -
gpg --keyserver pgp.mit.edu --recv-keys 2B5C1B00
gpg --export --armor 2B5C1B00 | sudo apt-key add -
gpg --keyserver pgp.mit.edu --recv-keys 0353B12C
gpg --export --armor 0353B12C | sudo apt-key add -
sudo apt-get update
sudo apt-get install cassandra

sudo addgroup --system cassandra
sudo adduser --system --home /ssd/cassandra --no-create-home --ingroup cassandra cassandra

export CASS_V=2.2

sudo ln -s /opt/apache-cassandra-$CASS_V /opt/cassandra

rm -rf /ssd/cassandra /ssd/log/cassandra
mkdir -p /ssd/cassandra /ssd/log/cassandra
sudo chown -R cassandra:cassandra /ssd/cassandra /ssd/log/cassandra

cp cassandra/cassandra.yaml cassandra/cassandra.yaml.mod
cat <<EOF > cassandra/cass_conf_replace.sed
s/- seeds: "\([^"]*\)"/- seeds: "$TFB_DBHOST"/
s/listen_address: \(.*\)/listen_address: $TFB_DBHOST/
s/rpc_address: \(.*\)/rpc_address: $TFB_DBHOST/
EOF
sed -i -f cassandra/cass_conf_replace.sed cassandra/cassandra.yaml.mod

sudo cp -f cassandra/cassandra.init /etc/init.d/cassandra
sudo cp -f cassandra/cassandra.init.env /etc/default/cassandra
sudo cp -f cassandra/cassandra.yaml.mod /opt/apache-cassandra-$CASS_V/conf/cassandra.yaml
sudo cp -f cassandra/log4j-server.properties /opt/apache-cassandra-$CASS_V/conf

sudo update-rc.d cassandra defaults
sudo service cassandra restart

for i in {1..30}; do
  nc -z $TFB_DBHOST 9160 && break || sleep 1;
  echo "Waiting for Cassandra ($i/30}"
done
nc -z $TFB_DBHOST 9160
if [ $? -eq 0 ]; then
  cat cassandra/cleanup-keyspace.cql | /opt/apache-cassandra-$CASS_V/bin/cqlsh $TFB_DBHOST
  python cassandra/db-data-gen.py > cassandra/tfb-data.cql
  /opt/apache-cassandra-$CASS_V/bin/cqlsh -f cassandra/create-keyspace.cql $TFB_DBHOST
  /opt/apache-cassandra-$CASS_V/bin/cqlsh -f cassandra/tfb-data.cql $TFB_DBHOST
else
  >&2 echo "Cassandra did not start, skipping"
fi

##############################
# Elasticsearch
##############################
echo "Setting up Elasticsearch"

export ES_V=1.5.0
#wget -nv https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-$ES_V.tar.gz
curl -Os https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-$ES_V.tar.gz
sudo tar zxf elasticsearch-$ES_V.tar.gz -C /opt
sudo ln -s /opt/elasticsearch-$ES_V /opt/elasticsearch

rm -rf /ssd/elasticsearch /ssd/log/elasticsearch
mkdir -p /ssd/elasticsearch /ssd/log/elasticsearch

sudo cp elasticsearch/elasticsearch.yml /opt/elasticsearch/config
sudo cp elasticsearch/elasticsearch /opt/elasticsearch

/opt/elasticsearch/elasticsearch restart

for i in {1..15}; do
  nc -z $TFB_DBHOST 9200 && break || sleep 1;
  echo "Waiting for Elasticsearch ($i/15}"
done
nc -z $TFB_DBHOST 9200
if [ $? -eq 0 ]; then
  sh elasticsearch/es-create-index.sh
  python elasticsearch/es-db-data-gen.py > elasticsearch/tfb-data.json
  curl -sS -D - -o /dev/null -XPOST localhost:9200/tfb/world/_bulk --data-binary @elasticsearch/tfb-data.json
  echo "Elasticsearch DB populated"
else
  >&2 echo "Elasticsearch did not start, skipping"
fi

##############################
# Redis
##############################
echo "Setting up Redis database"

sudo service redis-server stop
sudo mv redis.conf /etc/redis/redis.conf
sudo service redis-server start
bash create-redis.sh
rm create-redis.sh
