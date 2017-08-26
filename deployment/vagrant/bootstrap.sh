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

  sudo chown vagrant:vagrant ~/FrameworkBenchmarks
  cd ~/FrameworkBenchmarks

  # Set up the benchmark.cfg for vagrant user
cat <<EOF > benchmark.cfg
[Defaults]
# Available Keys:
os=linux
server_host=TFB-server
client_host=TFB-client
client_identity_file=/home/vagrant/.ssh/id_rsa
client_user=vagrant
database_host=TFB-database
database_identity_file=/home/vagrant/.ssh/id_rsa
database_os=linux
database_user=vagrant
duration=60
exclude=None
install=server
install_error_action=continue
install_strategy=unified
install_only=False
list_tests=False
concurrency_levels=[8, 16, 32, 64, 128, 256]
query_levels=[1, 5,10,15,20]
cached_query_levels=[1,10,20,50,100]
threads=8
mode=benchmark
sleep=60
test=None
type=all
verbose=True
clean=False
clean_all=False
ulimit=200000
EOF

  source ./toolset/setup/linux/prerequisites.sh

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

fi
