#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

sudo apt-get update
sudo apt-get install python-dev
sudo pip install pyopenssl --upgrade
sudo pip install ndg-httpsclient --upgrade
sudo pip install pyasn1 --upgrade
sudo pip install colorama
# Version 2.3 has a nice Counter() and other features
# but it requires —-allow-external and -—allow-unverified
sudo pip install progressbar
echo "127.0.0.1 " `hostname` | sudo tee /etc/hosts
echo "127.0.0.1 localhost" | sudo tee /etc/hosts

# Turn on command tracing
set -x

# Run as travis user (who already has passwordless sudo)
ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
cat /home/travis/.ssh/id_rsa.pub >> /home/travis/.ssh/authorized_keys
chmod og-wx /home/travis/.ssh/authorized_keys
echo "NoHostAuthenticationForLocalhost yes" | tee -a /home/travis/.ssh/config
chmod 600 ~/.ssh/config

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
echo "travis ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

# Set the default shell for testrunner to /bin/bash
sudo sed -i 's|/home/testrunner:/bin/sh|/home/testrunner:/bin/bash|g' /etc/passwd

ls -la /home/travis/.ssh
cat /home/travis/.ssh/config
sudo cat /etc/hosts
sudo cat /home/travis/.ssh/authorized_keys
