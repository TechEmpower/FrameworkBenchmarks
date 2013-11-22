#!/bin/bash  
#
# Bash script to be executed on the Linux server.
#
# Step 2: Additional setup.
#
echo "Host:" `hostname`
echo "Step 2: Additional setup"

export DEBIAN_FRONTEND=noninteractive
source ~/.bash_profile
source ~/benchmark-configuration.sh
source $BENCHMARK_HOME/toolset/deployment/common/bash-common.sh

ulimit -n 8192

echo ""
echo "INSTALL: Installing composer"
cd "$BENCHMARK_HOME/installs" || fail "Error changing directory."
curl -sS https://getcomposer.org/installer | php -- --install-dir=bin || fail "Error installing Composer."

echo ""
echo "INSTALL: Removing openjdk-6"
sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless -qq  || fail "Error removing openjdk-6."

echo ""
echo "INSTALL: Creating MongoDB database"
cd "$BENCHMARK_HOME" || fail "Error changing directory."
retry mongo --host $BENCHMARK_CLIENT_IP < config/create.js || fail "Error creating MongoDB database."

echo ""
echo "End of step 2"
