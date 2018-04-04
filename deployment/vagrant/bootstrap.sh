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
    $ docker run -it --network=tfb \
        -v /var/run/docker.sock:/var/run/docker.sock \
        --mount type=bind,source=`pwd`,target=/FrameworkBenchmarks \
        techempower/tfb --help

  You can run a test like:
    $ docker run -it --network=tfb \
        -v /var/run/docker.sock:/var/run/docker.sock \
        --mount type=bind,source=`pwd`,target=/FrameworkBenchmarks \
        techempower/tfb --mode verify --test gemini

  This Vagrant environment is already setup and ready to go.
EOF

  sudo mv motd /etc/
  sudo mv tfb /usr/local/bin
  sudo chmod a+x /usr/local/bin/tfb
  sudo chmod 777 /var/run/docker.sock
fi
