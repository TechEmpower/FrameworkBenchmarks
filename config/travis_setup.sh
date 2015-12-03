export DEBIAN_FRONTEND=noninteractive

# Turn on command tracing
set -x 

# Setup Apt For MongoDB
#   Due to TechEmpower/FrameworkBenchmarks#989 and travis-ci/travis-ci#2655, 
#   we put this into a loop
until timeout 15s sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10; do echo 'Waiting for apt-key' ; done
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list

# Setup apt for Apache Cassandra
until timeout 15s sudo apt-key adv --keyserver pgp.mit.edu --recv 4BD736A82B5C1B00; do echo 'Waiting for apt-key' ; done
sudo apt-add-repository  'deb http://www.apache.org/dist/cassandra/debian 20x main'

# Run installation 
# DO NOT COPY --force-yes TO ANY NON-TRAVIS-CI SCRIPTS! Seriously, it can cause some 
# major damage and should only be used inside a VM or Linux Container
sudo apt-get -q update
sudo apt-get -q -y --force-yes install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  mongodb-org \
  cassandra \
  openssh-server \
  mysql-server

# Run as travis user (who already has passwordless sudo)
ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
cat /home/travis/.ssh/id_rsa.pub > /home/travis/.ssh/authorized_keys
chmod 600 /home/travis/.ssh/authorized_keys

# Set up the benchmark.cfg for travis user
# NOTE: Please don't just copy the example config - it causes unexpected
#       issues when those example variables change
echo "[Defaults]"                                       > benchmark.cfg
echo "client_identity_file=/home/travis/.ssh/id_rsa"   >> benchmark.cfg
echo "database_identity_file=/home/travis/.ssh/id_rsa" >> benchmark.cfg
echo "client_host=127.0.0.1"                           >> benchmark.cfg
echo "database_host=127.0.0.1"                         >> benchmark.cfg
echo "server_host=127.0.0.1"                           >> benchmark.cfg
echo "client_user=travis"                              >> benchmark.cfg
echo "database_user=travis"                            >> benchmark.cfg
echo "runner_user=testrunner"                          >> benchmark.cfg

# Create the new testrunner user
sudo useradd testrunner
# Give him a home dir
sudo mkdir /home/testrunner
# Make testrunner the owner of his home dir
sudo chown testrunner:testrunner /home/testrunner
# Add the testrunner user to every group that the travis user is in
sudo sed -i 's|:travis|:travis,testrunner,benchmarkdbuser|g' /etc/group
# Maybe unneeded - add the travis user to the testrunner group
sudo sed -i 's|testrunner:x:\(.*\):|testrunner:x:\1:travis|g' /etc/group
# Need to add testrunner to the sudoers group AND default him to a sudoers
# because the travis user isn't in the sudo group - he's a sudoer.
echo "testrunner ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers
# Set the default shell for testrunner to /bin/bash
sudo sed -i 's|/home/testrunner:/bin/sh|/home/testrunner:/bin/bash|g' /etc/passwd

mkdir installs
sudo chown testrunner:testrunner installs

# =============Setup Databases===========================
# NOTE: Do not run `--install database` in travis-ci! 
#       It changes DB configuration files and will break everything
# =======================================================

# Setup MySQL
echo "Populating MySQL database"
#sudo mysqladmin -u root password secret
#sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
#sudo mv config/my.cnf /etc/mysql/my.cnf
sudo sed -i 's|#max_connections        = 100|max_connections        = 500|g' /etc/mysql/my.cnf
sudo restart mysql
#mysql -uroot -psecret < config/create.sql

# Setup Postgres
echo "Removing Postgres 9.1 from Travis-CI"
sudo apt-get remove -qy postgresql postgresql-9.1 postgresql-client-9.1
sudo apt-get install -qy postgresql-9.3 postgresql-client-9.3

echo "Populating Postgres database"
psql --version
sudo useradd benchmarkdbuser -p benchmarkdbpass
sudo -u postgres psql template1 < config/create-postgres-database.sql
sudo -u benchmarkdbuser psql hello_world < config/create-postgres.sql
sudo sed -i "s|#listen_addresses = 'localhost'|listen_addresses = '*'|g" /etc/postgresql/9.3/main/postgresql.conf
sudo sed -i 's|max_connections = 255|max_connections = 500|g' /etc/postgresql/9.3/main/postgresql.conf
sudo service postgresql stop
sudo service postgresql start 9.3

# Setup Apache Cassandra
echo "Populating Apache Cassandra database"
for i in {1..15}; do
nc -z localhost 9160 && break || sleep 1;
echo "Waiting for Cassandra ($i/15}"
done
nc -z localhost 9160
if [ $? -eq 0 ]; then
cat config/cassandra/cleanup-keyspace.cql | sudo cqlsh
python config/cassandra/db-data-gen.py > config/cassandra/tfb-data.cql
sudo cqlsh -f config/cassandra/create-keyspace.cql
sudo cqlsh -f config/cassandra/tfb-data.cql
else
>&2 echo "Cassandra did not start, skipping"
fi

# Setup Elasticsearch
curl -O https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.5.0.deb
sudo dpkg -i --force-confnew elasticsearch-1.5.0.deb
sudo update-rc.d elasticsearch defaults 95 10
sudo service elasticsearch restart

echo "Populating Elasticsearch database"
for i in {1..15}; do
nc -z localhost 9200 && break || sleep 1;
echo "Waiting for Elasticsearch ($i/15}"
done
nc -z localhost 9200
if [ $? -eq 0 ]; then
curl localhost:9200
sh config/elasticsearch/es-create-index.sh
python config/elasticsearch/es-db-data-gen.py > config/elasticsearch/tfb-data.json
curl -sS -D - -o /dev/null -XPOST localhost:9200/tfb/world/_bulk --data-binary @config/elasticsearch/tfb-data.json
echo "Elasticsearch DB populated"
else
>&2 echo "Elasticsearch did not start, skipping"
fi

# Setup MongoDB
echo "Populating MongoDB database"
for i in {1..15}; do
nc -z localhost 27017 && break || sleep 1;
echo "Waiting for MongoDB ($i/15}"
done
nc -z localhost 27017
if [ $? -eq 0 ]; then
mongo < config/create.js
mongod --version
else
>&2 echo "MongoDB did not start, skipping"
fi

# =============Modify Configurations===========================
# It can be useful to enable debug features for verification 
# inside Travis-CI
# =======================================================

sed -i 's|display_errors\] = off|display_errors\] = on|' config/php-fpm.conf

#exit $?
