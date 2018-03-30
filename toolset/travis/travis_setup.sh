#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

# Turn on command tracing
set -x

# Stop services that travis starts to free up those ports for docker usage
sudo service mysql stop
sudo service postgresql stop

# Run as travis user (who already has passwordless sudo)
ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
cat /home/travis/.ssh/id_rsa.pub >> /home/travis/.ssh/authorized_keys
chmod og-wx /home/travis/.ssh/authorized_keys
echo "NoHostAuthenticationForLocalhost yes" | tee -a /home/travis/.ssh/config
chmod 600 ~/.ssh/config

# Set up the benchmark.cfg for travis user
cp ./benchmark.cfg.example ./benchmark.cfg
sed -i s/techempower/travis/g ./benchmark.cfg

echo "travis ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

source ./toolset/setup/linux/prerequisites.sh

tfb --init --quiet