#!/bin/bash

fw_depends databases

RETCODE=$(fw_exists ${IROOT}/mongodb.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/mongodb.installed
  return 0; }

# send over the required files
scp $FWROOT/config/mongodb.conf $DBHOST:~/
scp $FWROOT/config/create.js $DBHOST:~/

# install mysql on database machine
ssh $DBHOST 'bash' <<EOF
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.0.list
sudo apt-get -y update
sudo apt-get -y remove mongodb-clients
sudo apt-get -y install -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' mongodb-org

nc -zvv $DBHOST 27017 &> /dev/null && sudo service mongod stop
sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
sudo cp mongodb.conf /etc/mongodb.conf
sudo mv mongodb.conf /etc/mongod.conf
sudo rm -rf /ssd/mongodb
sudo rm -rf /ssd/log/mongodb
sudo cp -R -p /var/lib/mongodb /ssd/
sudo cp -R -p /var/log/mongodb /ssd/log/
nc -zvv $DBHOST 27017 &> /dev/null || sudo service mongod start
EOF

echo -e "ssh \$DBHOST 'bash' <<EOF" > $IROOT/mongodb.installed
echo -e "mongo < create.js" >> $IROOT/mongodb.installed
echo -e "EOF" >> $IROOT/mongodb.installed

source $IROOT/mongodb.installed
