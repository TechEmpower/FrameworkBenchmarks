#!/bin/bash

fw_depends databases

RETCODE=$(fw_exists ${IROOT}/postgresql.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/postgresql.installed
  return 0; }

# delete any old required files that do not belong to us as
# scp will fail otherwise
ssh $DBHOST -t "
  sudo rm -rf create-postgres-database.sql
  sudo rm -rf create-postgres.sql
"

# send over the required files
scp $FWROOT/config/postgresql.conf $DBHOST:~/
scp $FWROOT/config/pg_hba.conf $DBHOST:~/
scp $FWROOT/config/60-postgresql-shm.conf $DBHOST:~/
scp $FWROOT/config/create-postgres-database.sql $DBHOST:~/
scp $FWROOT/config/create-postgres.sql $DBHOST:~/

# install postgresql on database machine
ssh $DBHOST -t 'sudo apt-get -y update
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" postgresql'

# This will support all 9.* versions depending on the machine
ssh $DBHOST -t "service postgresql status &> /dev/null
if [ $? -eq 0 ]; then
  sudo service postgresql stop
fi
sudo killall -s 9 -u postgres"

# Make sure all the configuration files in main belong to postgres
ssh $DBHOST -t "PG_VERSION=`pg_config --version | grep -oP '\d\.\d'`
sudo mv postgresql.conf /etc/postgresql/\${PG_VERSION}/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/\${PG_VERSION}/main/pg_hba.conf

sudo chown -Rf postgres:postgres /etc/postgresql/\${PG_VERSION}/main

sudo rm -rf /ssd/postgresql
sudo cp -R -p /var/lib/postgresql/\${PG_VERSION}/main /ssd/postgresql
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

sudo chown postgres:postgres /etc/sysctl.d/60-postgresql-shm.conf
sudo chown postgres:postgres create-postgres*

service postgresql status &> /dev/null
if [ $? -ne 0 ]; then
  sudo service postgresql start
fi"

echo -e "ssh \$DBHOST -t 'sudo -u postgres psql -q template1 < create-postgres-database.sql
  sudo -u postgres psql -q hello_world < create-postgres.sql'" > $IROOT/postgresql.installed

source $IROOT/postgresql.installed
