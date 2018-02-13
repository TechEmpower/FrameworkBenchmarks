#!/bin/bash

export DEBIAN_FRONTEND=noninteractive

#############################
# Prerequisites
#############################
sudo apt-get -y update

# WARNING: DON'T PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
# Dpkg::Options avoid hangs on Travis-CI, doesn't affect clean systems
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  linux-image-extra-$(uname -r) `# Needed for Docker on Ubuntu 14` \
  linux-image-extra-virtual     `# Needed for Docker on Ubuntu 14` \
  apt-transport-https           `# Needed for Docker on Ubuntu 14` \
  ca-certificates               `# Needed for Docker on Ubuntu 14` \
  curl                          `# Needed for Docker on Ubuntu 14` \
  software-properties-common    `# Needed for Docker on Ubuntu 14`

# TODO: this is likely going away with docker implementation (docker provides a ulimit cli arg)
sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

#
# Install Docker
#
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update
sudo apt-get -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold"  docker-ce
if ! sudo grep -q -E "^docker:" /etc/group; then
  sudo groupadd docker
  sudo usermod -aG docker $USER
fi
