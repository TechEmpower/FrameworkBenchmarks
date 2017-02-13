#!/usr/bin/env bash
#
# Prepares a virtual machine for running TFB

# A shell provisioner is called multiple times
if [ ! -e "~/.firstboot" ]; then

  # Workaround mitchellh/vagrant#289
  echo "grub-pc grub-pc/install_devices multiselect     /dev/sda" | sudo debconf-set-selections

  # Install prerequisite tools
  echo "Installing prerequisites"
  sudo apt-get update
  sudo apt-get install -y git
  git config --global core.autocrlf input

  # Setting up ssh & passwordless sudo
  ssh-keygen -f /home/vagrant/.ssh/id_rsa -N '' -t rsa
  cat /home/vagrant/.ssh/id_rsa.pub >> /home/vagrant/.ssh/authorized_keys
  chmod og-wx /home/vagrant/.ssh/authorized_keys
  echo "NoHostAuthenticationForLocalhost yes" | tee -a /home/vagrant/.ssh/config
  chmod 600 ~/.ssh/config
  echo "vagrant ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers

  # Setting up hosts aliases
  echo 127.0.0.1 TFB-database | sudo tee --append /etc/hosts
  echo 127.0.0.1 TFB-client   | sudo tee --append /etc/hosts
  echo 127.0.0.1 TFB-server   | sudo tee --append /etc/hosts

  # Setting up new FWROOT
  export FWROOT="/home/vagrant/FrameworkBenchmarks"
  echo `export FWROOT="/home/vagrant/FrameworkBenchmarks"` >> ~/.bashrc

  cd ~/FrameworkBenchmarks

  # Set up the benchmark.cfg for vagrant user
  cp ./benchmark.cfg.example ./benchmark.cfg
  sed -i s/techempower/vagrant/g benchmark.cfg

  source ./toolset/setup/linux/prerequisites.sh

  # Setup a nice welcome message for our guest
  echo "Setting up welcome message"
  sudo rm -f /etc/update-motd.d/51-cloudguest
  sudo rm -f /etc/update-motd.d/98-cloudguest
  sudo mv ~/.custom_motd.sh /etc/update-motd.d/55-tfbwelcome
fi
