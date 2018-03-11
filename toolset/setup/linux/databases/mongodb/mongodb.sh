#!/bin/bash

fw_depends databases

fw_installed mongodb && return 0

# send over the required files
scp $FWROOT/toolset/setup/linux/databases/mongodb/mongodb.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mongodb/create.js $DBHOST:~/

# install mongo on database machine
ssh $DBHOST 'bash' <<"EOF"
echo "Setting up MongoDB database"

if service --status-all | grep -Fq 'mongod'; then
  sudo service mongod stop
fi
sudo apt-get -y remove --purge mongodb-org
sudo apt-get -y autoremove
sudo find /etc/apt/sources.list.d -type f -name 'mongodb-org-*.list' -delete -maxdepth 1
sudo find /etc -type f -name 'mongo*.conf*' -delete -maxdepth 1
sudo rm -rf /var/lib/mongodb
sudo rm -rf /var/log/mongodb

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2930ADAE8CAF5059EE73BB4B58712A2291FA4AD5
echo "deb [ arch=amd64 ] https://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.6 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.6.list
sudo apt-get -y update
sudo apt-get -y install mongodb-org

sudo service mongod stop
sudo mv mongodb.conf /etc/mongod.conf
sudo rm -rf /ssd/mongodb
sudo rm -rf /ssd/log/mongodb
sudo mkdir -p /ssd/mongodb
sudo mkdir -p /ssd/log/mongodb
sudo chown -R mongodb:mongodb /ssd/mongodb
sudo chown -R mongodb:mongodb /ssd/log/mongodb
sudo service mongod start

for i in {1..15}; do
  nc -z localhost 27017 && break || sleep 1;
  echo "Waiting for MongoDB ($i/15)"
done
nc -z localhost 27017
if [ $? -eq 0 ]; then
  mongo < create.js
  mongod --version
else
  >&2 echo "MongoDB did not start, skipping"
fi
EOF

echo -e "ssh \$DBHOST 'bash' <<\"EOF\"" > $IROOT/mongodb.installed
echo -e "sudo service mongod start || echo 'mongod service already started'" >> $IROOT/mongodb.installed
echo -e "for i in {1..15}; do" >> $IROOT/mongodb.installed
echo -e "  nc -z localhost 27017 && break || sleep 1;" >> $IROOT/mongodb.installed
echo -e "  echo \"Waiting for MongoDB (\$i/15)\"" >> $IROOT/mongodb.installed
echo -e "done" >> $IROOT/mongodb.installed
echo -e "mongo < create.js" >> $IROOT/mongodb.installed
echo -e "EOF" >> $IROOT/mongodb.installed

source $IROOT/mongodb.installed
