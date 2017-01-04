#!/bin/bash

fw_depends databases

fw_installed mysql && return 0

# send over the required files
scp $FWROOT/toolset/setup/linux/databases/mysql/create.sql $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/mysql $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/mysql.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/my.cnf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mysql/usr.sbin.mysqld $DBHOST:~/

# install mysql on database machine
ssh $DBHOST 'bash' <<EOF
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

mysql -uroot -psecret -e'quit' &> /dev/null || sudo mysqladmin -u root password secret
EOF

# Install the mysql client
sudo apt-get install -y mysql-client

echo -e "ssh \$DBHOST 'bash' <<EOF" > $IROOT/mysql.installed
echo -e "mysql -uroot -psecret < create.sql" >> $IROOT/mysql.installed
echo -e "EOF" >> $IROOT/mysql.installed

source $IROOT/mysql.installed
