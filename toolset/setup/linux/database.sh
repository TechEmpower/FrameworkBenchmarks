#!/bin/bash
#
# Configures the database server for TFB
#
# Note: This is not used for Travis-CI. See run-ci.py to see
# how databases are configured for Travis.
#
# Note on Compatibility: TFB *only* supports Ubuntu 14.04 64bit
# (e.g. trusty64). However, it's nice to retain 12.04 support 
# where possible, as it's still heavily used. 
#
# Database setup is one core area where we can help ensure TFB 
# works on 12.04 with minimal frustration. In some cases we  
# manually install the DB version that's expected, instead of the 
# 12.04 default. In other cases we can use a 12.04 specific 
# configuration file. These patches are not intended to enable 
# benchmarking (e.g. there are no guarantees that 
# the databases will be tuned for performance correctly), but 
# they do allow users on 12.04 to install and run most TFB tests. 
# Some tests internally have 12.04 incompatibilities, we make no 
# concentrated effort to address these cases, but PR's for specific 
# problems are welcome

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
sudo sh -c "echo mysql-server mysql-server/root_password_again select secret | debconf-set-selections"
sudo sh -c "echo mysql-server mysql-server/root_password select secret | debconf-set-selections"

sudo apt-get -y install mysql-server

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

# Insert data
mysql -uroot -psecret < create.sql
rm create.sql

##############################
# Postgres
##############################
echo "Setting up Postgres database"
if [ "$TFB_DISTRIB_CODENAME" == "precise" ]; then
  echo "WARNING: Force upgrading Postgres for Ubuntu 12.04"
  sudo apt-get remove -y postgresql postgresql-9.1 postgresql-client-9.1

  echo "deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main" | sudo tee /etc/apt/sources.list.d/pgdg.list
  wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
  sudo apt-get update
  sudo apt-get install -y postgresql-9.3 postgresql-client-9.3
  sudo service postgresql start
fi
sudo service postgresql stop
# Sometimes this doesn't work with postgresql
sudo killall -s 9 -u postgres
sudo mv postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf

sudo rm -rf /ssd/postgresql
sudo cp -R -p /var/lib/postgresql/9.3/main /ssd/postgresql
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

sudo service postgresql start

sudo -u postgres psql template1 < create-postgres-database.sql
sudo -u benchmarkdbuser psql hello_world < create-postgres.sql
rm create-postgres-database.sql create-postgres.sql
# Last chance to make sure postgresql starts up correctly
sudo killall -s 9 -u postgres
sudo service postgresql restart

##############################
# MongoDB
#
# Note for 12.04: Using mongodb.org ensures 2.6 is installed
##############################
echo "Setting up MongoDB database"
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list
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
sudo apt-get install -qqy openjdk-7-jdk

sudo addgroup --system cassandra
sudo adduser --system --home /ssd/cassandra --no-create-home --ingroup cassandra cassandra

export CASS_V=2.0.12
wget -nv http://archive.apache.org/dist/cassandra/$CASS_V/apache-cassandra-$CASS_V-bin.tar.gz
sudo tar xzf apache-cassandra-$CASS_V-bin.tar.gz -C /opt
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

for i in {1..15}; do
  nc -z $TFB_DBHOST 9160 && break || sleep 1;
  echo "Waiting for Cassandra ($i/15}"
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
wget -nv https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-$ES_V.tar.gz
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
if [ "$TFB_DISTRIB_CODENAME" == "precise" ]; then
  echo "WARNING: Downgrading Redis configuration for Ubuntu 12.04"

  # On 12.04, Redis 2.4 is installed. It doesn't support 
  # some of the 2.6 options, so we have to remove or comment
  # those
  sed -i 's/tcp-keepalive/# tcp-keepalive/' redis.conf
  sed -i 's/stop-writes-on-bgsave-error/# stop-writes-on-bgsave-error/' redis.conf
  sed -i 's/rdbchecksum/# rdbchecksum/' redis.conf
  sed -i 's/slave-read-only/# slave-read-only/' redis.conf
  sed -i 's/repl-disable-tcp-nodelay/# repl-disable-tcp-nodelay/' redis.conf
  sed -i 's/slave-priority/# slave-priority/' redis.conf
  sed -i 's/auto-aof-rewrite-percentage/# auto-aof-rewrite-percentage/' redis.conf
  sed -i 's/auto-aof-rewrite-min-size/# auto-aof-rewrite-min-size/' redis.conf
  
  sed -i 's/lua-time-limit/# lua-time-limit/' redis.conf
  sed -i 's/notify-keyspace-events/# notify-keyspace-events/' redis.conf
  sed -i 's/hash-max-ziplist-entries/# hash-max-ziplist-entries/' redis.conf
  sed -i 's/hash-max-ziplist-value/# hash-max-ziplist-value/' redis.conf
  sed -i 's/zset-max-ziplist-entries/# zset-max-ziplist-entries/' redis.conf
  sed -i 's/zset-max-ziplist-value/# zset-max-ziplist-value/' redis.conf
  sed -i 's/client-output-buffer-limit/# client-output-buffer-limit/' redis.conf
 
  sed -i 's/hz 10/# hz 10/' redis.conf
  sed -i 's/aof-rewrite-incremental-fsync/# aof-rewrite-incremental-fsync/' redis.conf
fi

sudo service redis-server stop
sudo mv redis.conf /etc/redis/redis.conf
sudo service redis-server start
bash create-redis.sh
rm create-redis.sh
