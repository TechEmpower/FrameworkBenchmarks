#!/usr/bin/env bash
#
# Prepares a virtual machine for running TFB

# A shell provisioner is called multiple times
if [ ! -e "~/.firstboot" ]; then

  # Workaround mitchellh/vagrant#289
  echo "grub-pc grub-pc/install_devices multiselect     /dev/sda" | sudo debconf-set-selections

  # Install prerequisite tools
  echo "Installing docker"
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
  sudo apt-get update
  sudo apt-get install -yqq docker-ce

  # Setting up passwordless sudo
  echo "vagrant ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

  sudo chown vagrant:vagrant ~/FrameworkBenchmarks
  cd ~/FrameworkBenchmarks

  # Setup a nice welcome message for our guest
  echo "Setting up welcome message"
  sudo rm -f /etc/update-motd.d/51-cloudguest
  sudo rm -f /etc/update-motd.d/98-cloudguest

  sudo cat <<EOF > motd
Welcome to the FrameworkBenchmarks project!

  To get started, perhaps try this:
    $ cd FrameworkBenchmarks

  You can get lots of help:
    $ tfb --help

  You can run a test like:
    $ tfb --mode verify --test gemini

  This Vagrant environment is already setup and ready to go.
EOF

  sudo mv motd /etc/

  sudo cat <<EOF > tfb
#!/bin/bash

# Defaults
ds=/var/run/docker.sock
sd=/home/vagrant/FrameworkBenchmarks

# Build the tfb image
docker pull techempower/tfb

# Create the tfb network
docker network create tfb > /dev/null 2>&1
# Run the suite
docker run --network=tfb -v \${DOCKER_SOCKET_PATH-\$ds}:/var/run/docker.sock --mount type=bind,source=\${TFB_SOURCE_DIR-\$sd},target=/FrameworkBenchmarks techempower/tfb "\$@"
EOF
  sudo mv tfb /usr/local/bin
  sudo chmod a+x /usr/local/bin/tfb
  sudo chmod 777 /var/run/docker.sock
fi
