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

export DB_HOST={database_host}

set -x
export DEBIAN_FRONTEND=noninteractive

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

CODENAME=$(lsb_release -sc)

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
if [ "$CODENAME" == "precise" ]; then
  echo "WARNING: Force upgrading Postgres for Ubuntu 12.04"
  sudo apt-get remove -y postgresql postgresql-9.1 postgresql-client-9.1

  echo "deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main" | sudo tee /etc/apt/sources.list.d/pgdg.list
  wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
  sudo apt-get update
  sudo apt-get install -y postgresql-9.3 postgresql-client-9.3
  sudo -u postgres -H /etc/init.d/postgresql start
fi

sudo -u postgres psql template1 < create-postgres-database.sql
sudo -u benchmarkdbuser psql hello_world < create-postgres.sql
rm create-postgres-database.sql create-postgres.sql

sudo -u postgres -H /etc/init.d/postgresql stop
sudo mv postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf

sudo cp -R -p /var/lib/postgresql/9.3/main /ssd/postgresql
sudo -u postgres -H /etc/init.d/postgresql start
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

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
sudo cp -R -p /var/lib/mongodb /ssd/
sudo cp -R -p /var/log/mongodb /ssd/log/
sudo service mongod start

until nc -z localhost 27017 ; do echo Waiting for MongoDB; sleep 1; done
mongo < create.js
rm create.js
mongod --version

##############################
# Apache Cassandra
##############################
echo "Setting up Apache Cassandra database"
sudo apt-get install -qqy openjdk-7-jdk
export CASS_V=2.0.7
wget -nv http://archive.apache.org/dist/cassandra/$CASS_V/apache-cassandra-$CASS_V-bin.tar.gz
tar xzf apache-cassandra-$CASS_V-bin.tar.gz

rm -rf /ssd/cassandra /ssd/log/cassandra
mkdir -p /ssd/cassandra /ssd/log/cassandra

sed -i "s/^.*seeds:.*/          - seeds: \"$DB_HOST\"/" cassandra/cassandra.yaml
sed -i "s/^listen_address:.*/listen_address: $DB_HOST/" cassandra/cassandra.yaml
sed -i "s/^rpc_address:.*/rpc_address: $DB_HOST/" cassandra/cassandra.yaml

mv cassandra/cassandra.yaml apache-cassandra-$CASS_V/conf
mv cassandra/log4j-server.properties apache-cassandra-$CASS_V/conf
nohup apache-cassandra-$CASS_V/bin/cassandra -p c.pid > cassandra.log

until nc -z $DB_HOST 9160 ; do echo Waiting for Cassandra; sleep 1; done
cat cassandra/cleanup-keyspace.cql | apache-cassandra-$CASS_V/bin/cqlsh $DB_HOST
python cassandra/db-data-gen.py > cassandra/tfb-data.cql
apache-cassandra-$CASS_V/bin/cqlsh -f cassandra/create-keyspace.cql $DB_HOST
apache-cassandra-$CASS_V/bin/cqlsh -f cassandra/tfb-data.cql $DB_HOST
rm -rf apache-cassandra-*-bin.tar.gz cassandra

##############################
# Redis
##############################
echo "Setting up Redis database"
if [ "$CODENAME" == "precise" ]; then
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
