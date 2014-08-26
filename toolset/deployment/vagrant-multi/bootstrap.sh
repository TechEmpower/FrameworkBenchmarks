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

# Setup hosts 
echo Setting up convenience hosts entries
echo 172.16.0.18 TFB-database | sudo tee --append /etc/hosts
echo 172.16.0.17 TFB-client | sudo tee --append /etc/hosts
echo 172.16.0.16 TFB-server | sudo tee --append /etc/hosts

# Update hostname to reflect our current role
echo Updating hostname
echo 127.0.0.1 `hostname` | sudo tee --append /etc/hosts
myhost=TFB-${1}
echo $myhost | sudo tee --append /etc/hostname
sudo hostname $myhost
echo Updated /etc/hosts file to be: 
cat /etc/hosts

# Workaround mitchellh/vagrant#289
echo "grub-pc grub-pc/install_devices multiselect     /dev/sda" | sudo debconf-set-selections

# Install prerequisite tools
echo "Installing prerequisites"
sudo apt-get install -y git
sudo apt-get install -y python-pip

# Initial project setup
if [ -d "/FrameworkBenchmarks" ]; then
  ln -s /FrameworkBenchmarks $FWROOT
  echo "Removing any install or results files so they do not interfere"
  rm -rf $FWROOT/installs $FWROOT/results
else
  # If there is no synced folder, clone the project
  echo "Cloning project from TechEmpower/FrameworkBenchmarks master"
  git clone https://github.com/TechEmpower/FrameworkBenchmarks.git $FWROOT
fi
sudo pip install -r $FWROOT/config/python_requirements.txt

# Everyone gets SSH access to localhost
echo "Setting up SSH access to localhost"
ssh-keygen -t rsa -N '' -f ~/.ssh/id_rsa
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
chmod 600 ~/.ssh/authorized_keys

# Ensure keys have proper permissions
chmod 600 ~/.ssh/client ~/.ssh/database

# Database and client need to add their specific keys
# Note : this are always copied from the local working copy using a
#        file provisioner. While they exist in the git clone we just 
#        created (so we could use those), we want to let the user
#        have the option of replacing the keys in their working copy
#        and ensuring that only they can ssh into the machines
mykey=~/.ssh/${1}.pub
if [ -e $mykey ]; then
  echo "Setting up SSH access for the TFB-server"
  echo "Using key: "
  ssh-keygen -lv -f $mykey
  cat $mykey >> ~/.ssh/authorized_keys
fi

# Setup 
echo "Running software installation for TFB-${1}"
cd $FWROOT
toolset/run-tests.py --verbose --install $1 --install-only --test ''

# Setup a nice welcome message for our guest
echo "Setting up welcome message"
sudo rm -f /etc/update-motd.d/51-cloudguest
sudo rm -f /etc/update-motd.d/98-cloudguest
sudo cp /vagrant/custom_motd.sh /etc/update-motd.d/55-tfbwelcome


