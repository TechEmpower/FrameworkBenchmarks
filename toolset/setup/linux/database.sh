#!/bin/bash

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
    redis-server      `# Installs 9.1 or 9.3, based on Ubuntu version`

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
sudo -u postgres psql template1 < create-postgres-database.sql
sudo -u benchmarkdbuser psql hello_world < create-postgres.sql
rm create-postgres-database.sql create-postgres.sql

sudo -u postgres -H /etc/init.d/postgresql stop
# NOTE: This will cause errors on Ubuntu 12.04, as apt installs 
# an older version (9.1 instead of 9.3)
sudo mv postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf

sudo cp -R -p /var/lib/postgresql/9.3/main /ssd/postgresql
sudo -u postgres -H /etc/init.d/postgresql start
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

##############################
# MongoDB
##############################
echo "Setting up MongoDB database"
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list
sudo apt-get -y update
sudo apt-get -y remove mongodb-clients
sudo apt-get -y install mongodb-org

sudo service mongod stop
sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
sudo mv mongodb.conf /etc/mongodb.conf
sudo mv mongodb.conf /etc/mongod.conf
sudo cp -R -p /var/lib/mongodb /ssd/
sudo cp -R -p /var/log/mongodb /ssd/log/
sudo service mongod start

until nc -z localhost 27017 ; do echo Waiting for MongoDB; sleep 1; done
mongo < create.js
rm create.js

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
sudo service redis-server stop
# NOTE: This conf will cause errors on Ubuntu 12.04, as apt installs 
# an older version of redis
sudo mv redis.conf /etc/redis/redis.conf
sudo service redis-server start
bash create-redis.sh
rm create-redis.sh
