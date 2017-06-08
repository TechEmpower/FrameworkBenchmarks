#!/bin/bash

fw_depends databases

fw_installed mysql && return 0

# send over the required files
scp $FWROOT/toolset/setup/linux/databases/mysql/create.sql $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/my.cnf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/usr.sbin.mysqld $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/mysql.list $DBHOST:~/

# install mysql on database machine
ssh $DBHOST 'bash' <<EOF
sudo cp mysql.list /etc/apt/sources.list.d/
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 8C718D3B5072E1F5
sudo apt-get update
sudo debconf-set-selections <<< "mysql-community-server mysql-community-server/data-dir select 'Y'"
sudo debconf-set-selections <<< "mysql-community-server mysql-community-server/root-pass password secret"
sudo debconf-set-selections <<< "mysql-community-server mysql-community-server/re-root-pass password secret"
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server

sudo service mysql stop

sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
sudo cp my.cnf /etc/mysql/my.cnf

sudo rm -rf /ssd/mysql
sudo rm -rf /ssd/log/mysql
sudo cp -R -p /var/lib/mysql /ssd/
sudo cp -R -p /var/log/mysql /ssd/log
sudo cp usr.sbin.mysqld /etc/apparmor.d/
sudo /etc/init.d/apparmor reload
sudo service mysql start
EOF

echo -e "ssh \$DBHOST 'bash' <<EOF" > $IROOT/mysql.installed
echo -e "sudo service mysql start || echo 'mysql service already started'" >> $IROOT/mysql.installed
echo -e "mysqladmin -uroot -psecret flush-hosts" >> $IROOT/mysql.installed
echo -e "mysql -uroot -psecret < create.sql" >> $IROOT/mysql.installed
echo -e "EOF" >> $IROOT/mysql.installed

source $IROOT/mysql.installed
