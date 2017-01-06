#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

# Turn on command tracing
set -x

# Run as travis user (who already has passwordless sudo)
ssh-keygen -f /home/travis/.ssh/id_rsa -N '' -t rsa
cat /home/travis/.ssh/id_rsa.pub >> /home/travis/.ssh/authorized_keys
chmod og-wx /home/travis/.ssh/authorized_keys
echo "NoHostAuthenticationForLocalhost yes" | tee -a /home/travis/.ssh/config
chmod 600 ~/.ssh/config

# Set up the benchmark.cfg for travis user
echo "[Defaults]"                                       > benchmark.cfg
echo "client_identity_file=/home/travis/.ssh/id_rsa"   >> benchmark.cfg
echo "database_identity_file=/home/travis/.ssh/id_rsa" >> benchmark.cfg
echo "client_host=127.0.0.1"                           >> benchmark.cfg
echo "database_host=127.0.0.1"                         >> benchmark.cfg
echo "server_host=127.0.0.1"                           >> benchmark.cfg
echo "client_user=travis"                              >> benchmark.cfg
echo "database_user=travis"                            >> benchmark.cfg
echo "runner_user=travis"                              >> benchmark.cfg

echo "travis ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

source ./toolset/setup/linux/prerequisites.sh
