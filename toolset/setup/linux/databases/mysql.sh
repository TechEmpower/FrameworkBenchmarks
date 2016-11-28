#!/bin/bash

fw_depends databases

RETCODE=$(fw_exists ${IROOT}/mysql.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/mysql.installed
  return 0; }

# send over the required files
scp $FWROOT/config/create.sql $DBHOST:~/
scp $FWROOT/config/mysql $DBHOST:~/
scp $FWROOT/config/mysql.conf $DBHOST:~/
scp $FWROOT/config/my.cnf $DBHOST:~/
scp $FWROOT/config/usr.sbin.mysqld $DBHOST:~/

# install mysql on database machine
ssh $DBHOST -t "
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server

sudo stop mysql

sudo mv mysql /etc/init.d/mysql
sudo chmod +x /etc/init.d/mysql
sudo mv mysql.conf /etc/init/mysql.conf

sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
sudo mv my.cnf /etc/mysql/my.cnf

sudo rm -rf /ssd/mysql
sudo rm -rf /ssd/log/mysql
sudo cp -R -p /var/lib/mysql /ssd/
sudo cp -R -p /var/log/mysql /ssd/log
sudo cp usr.sbin.mysqld /etc/apparmor.d/
sudo /etc/init.d/apparmor reload
sudo start mysql

if ! mysql -uroot -psecret -e'quit' &> /dev/null; then
  sudo mysqladmin -u root password secret
fi"

# Install the mysql client
sudo apt-get install -y mysql-client

echo -e "ssh \$DBHOST -t 'mysql -uroot -psecret < create.sql'" > $IROOT/mysql.installed

source $IROOT/mysql.installed
