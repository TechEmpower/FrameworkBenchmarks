#!/bin/bash

fw_depends databases

fw_installed postgresql && return 0

# delete any old required files that do not belong to us as
# scp will fail otherwise
ssh $DBHOST 'bash' <<EOF
  sudo rm -rf create-postgres-database.sql
  sudo rm -rf create-postgres.sql
EOF

# send over the required files
scp $FWROOT/toolset/setup/linux/databases/postgresql/postgresql.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/postgresql/pg_hba.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/postgresql/60-postgresql-shm.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/postgresql/create-postgres-database.sql $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/postgresql/create-postgres.sql $DBHOST:~/

ssh $DBHOST 'bash' <<EOF
# install postgresql on database machine
sudo apt-get -y update
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" postgresql

# This will support all 9.* versions depending on the machine
service postgresql status &> /dev/null && sudo service postgresql stop
# Because postgresql...
sudo killall -9 -u postgres

# Make sure all the configuration files in main belong to postgres
PG_VERSION=`pg_config --version | grep -oP '\d\.\d'`
sudo mv postgresql.conf /etc/postgresql/\${PG_VERSION}/main/postgresql.conf
sudo mv pg_hba.conf /etc/postgresql/\${PG_VERSION}/main/pg_hba.conf

sudo chown -Rf postgres:postgres /etc/postgresql/\${PG_VERSION}/main

sudo rm -rf /ssd/postgresql
sudo cp -R -p /var/lib/postgresql/\${PG_VERSION}/main /ssd/postgresql
sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

sudo chown postgres:postgres /etc/sysctl.d/60-postgresql-shm.conf
sudo chown postgres:postgres create-postgres*
EOF

echo -e "ssh \$DBHOST <<EOF" > $IROOT/postgresql.installed
echo "service postgresql status &> /dev/null || sudo service postgresql start" >> $IROOT/postgresql.installed
echo "sudo -u postgres psql -q template1 < create-postgres-database.sql" >> $IROOT/postgresql.installed
echo "sudo -u postgres psql -q hello_world < create-postgres.sql" >> $IROOT/postgresql.installed
echo "EOF" >> $IROOT/postgresql.installed

source $IROOT/postgresql.installed
