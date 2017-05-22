#!/bin/bash

fw_depends databases

fw_installed mongodb && return 0

# send over the required files
scp $FWROOT/toolset/setup/linux/databases/mongodb/mongodb.conf $DBHOST:~/
scp $FWROOT/toolset/setup/linux/databases/mongodb/create.js $DBHOST:~/

# install mongo on database machine
ssh $DBHOST 'bash' <<EOF
echo "Setting up MongoDB database"
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
echo 'deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse' | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
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
  mongod --version
else
  >&2 echo "MongoDB did not start, skipping"
fi
EOF

echo -e "ssh \$DBHOST 'bash' <<EOF" > $IROOT/mongodb.installed
echo -e "sudo service mongod start || echo 'mongod service already started'" >> $IROOT/mongodb.installed
echo -e "mongo < create.js" >> $IROOT/mongodb.installed
echo -e "EOF" >> $IROOT/mongodb.installed

source $IROOT/mongodb.installed
