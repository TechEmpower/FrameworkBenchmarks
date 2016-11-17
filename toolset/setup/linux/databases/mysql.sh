#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/mysql.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/mysql.installed
  return 0; }

# install mysql on database machine
ssh $DBHOST -t "
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server \
\
sudo stop mysql \
# disable checking of disk size \
sudo mv mysql /etc/init.d/mysql \
sudo chmod +x /etc/init.d/mysql \
sudo mv mysql.conf /etc/init/mysql.conf \
# use the my.cnf file to overwrite /etc/mysql/my.cnf \
sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig \
sudo mv my.cnf /etc/mysql/my.cnf \
\
sudo rm -rf /ssd/mysql \
sudo rm -rf /ssd/log/mysql \
sudo cp -R -p /var/lib/mysql /ssd/ \
sudo cp -R -p /var/log/mysql /ssd/log \
sudo cp usr.sbin.mysqld /etc/apparmor.d/ \
sudo /etc/init.d/apparmor reload \
sudo start mysql \
\
# Set root password \
sudo mysqladmin -u root password secret"

# Install the mysql client
sudo apt-get install -y mysql-client

echo -e "mysql -uroot -psecret -h \$DBHOST < \$FWROOT/config/create.sql" > $IROOT/mysql.installed

source $IROOT/mysql.installed
