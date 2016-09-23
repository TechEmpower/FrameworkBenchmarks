#!/usr/bin/env bash

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

# Export some variables so you can copy/paste the rest.
export TFB_DATABASE_USER=travis
export TFB_DATABASE_HOST=127.0.0.1
export TFB_CLIENT_USER=travis
export TFB_CLIENT_HOST=127.0.0.1

# Set up the database machine for SSH
cat ~/.ssh/id_rsa.pub | ssh $TFB_DATABASE_USER@$TFB_DATABASE_HOST 'mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys'
scp ~/.ssh/id_rsa $TFB_DATABASE_USER@$TFB_DATABASE_HOST:~/.ssh/id_rsa
scp ~/.ssh/id_rsa.pub $TFB_DATABASE_USER@$TFB_DATABASE_HOST:~/.ssh/id_rsa.pub
# Set up the client machine for SSH
cat ~/.ssh/id_rsa.pub | ssh $TFB_CLIENT_USER@$TFB_CLIENT_HOST 'mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys'
scp ~/.ssh/id_rsa $TFB_CLIENT_USER@$TFB_CLIENT_HOST:~/.ssh/id_rsa
scp ~/.ssh/id_rsa.pub $TFB_CLIENT_USER@$TFB_CLIENT_HOST:~/.ssh/id_rsa.pub

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

