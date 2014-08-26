#!/usr/bin/env bash
#
# Prepares a virtual machine for running TFB
#
# Intentionally uses ~, $HOME, and $USER so that the 
# same script can work for VirtualBox (username vagrant)
# and Amazon (username ubuntu)

# Setup some nice TFB defaults
echo "export TFB_SERVER_HOST=172.16.0.16" >> ~/.bash_profile
echo "export TFB_CLIENT_HOST=172.16.0.17" >> ~/.bash_profile
echo "export TFB_DATABASE_HOST=172.16.0.18" >> ~/.bash_profile
echo "export TFB_CLIENT_USER=$USER" >> ~/.bash_profile
echo "export TFB_DATABASE_USER=$USER" >> ~/.bash_profile
echo "export TFB_CLIENT_IDENTITY_FILE=$HOME/.ssh/client" >> ~/.bash_profile
echo "export TFB_DATABASE_IDENTITY_FILE=$HOME/.ssh/database" >> ~/.bash_profile
echo "export FWROOT=$HOME/FrameworkBenchmarks" >> ~/.bash_profile 
if [ -e "~/FrameworkBenchmarks/benchmark.cfg" ]; then
  echo "You have a benchmark.cfg file that will interfere with Vagrant, moving to benchmark.cfg.bak"
  mv ~/FrameworkBenchmarks/benchmark.cfg ~/FrameworkBenchmarks/benchmark.cfg.bak
fi
source ~/.bash_profile

# Enable SSH to localhost
ssh-keygen -t rsa -N '' -f ~/.ssh/id_rsa
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

# Workaround mitchellh/vagrant#289
echo "grub-pc grub-pc/install_devices multiselect     /dev/sda" | sudo debconf-set-selections

# Install prerequisite tools
sudo apt-get install -y git
sudo apt-get install -y python-pip

# Initial project setup
if [ -d "/FrameworkBenchmarks" ]; then
  ln -s /FrameworkBenchmarks $FWROOT
  echo "Removing any install or results files so they do not interfere"
  rm -rf $FWROOT/installs $FWROOT/results
else
  # If there is no synced folder, clone the project
  git clone https://github.com/TechEmpower/FrameworkBenchmarks.git $FWROOT
fi
sudo pip install -r $FWROOT/config/python_requirements.txt

# Setup 
cd $FWROOT
toolset/run-tests.py --verbose --install $1 --install-only --test ''

# Setup a nice welcome message for our guest
sudo rm -f /etc/update-motd.d/51-cloudguest
sudo rm -f /etc/update-motd.d/98-cloudguest
sudo cp /vagrant/custom_motd.sh /etc/update-motd.d/55-tfbwelcome

touch ~/.firstboot


