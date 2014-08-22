#!/usr/bin/env bash
if [ ! -e "/home/vagrant/.firstboot" ]; then

  # Setup some nice TFB defaults
  echo "export TFB_SERVER_HOST=127.0.0.1" >> ~/.bash_profile
  echo "export TFB_CLIENT_HOST=127.0.0.1" >> ~/.bash_profile
  echo "export TFB_DATABASE_HOST=127.0.0.1" >> ~/.bash_profile
  echo "export TFB_CLIENT_USER=vagrant" >> ~/.bash_profile
  echo "export TFB_DATABASE_USER=vagrant" >> ~/.bash_profile
  echo "export TFB_CLIENT_IDENTITY_FILE=/home/vagrant/.ssh/id_rsa" >> ~/.bash_profile
  echo "export TFB_DATABASE_IDENTITY_FILE=/home/vagrant/.ssh/id_rsa" >> ~/.bash_profile
  if [ "$1" == "dev" ]; then
    echo "FrameworkBenchmarks: Running in developer mode"
    echo "export FWROOT=/FrameworkBenchmarks" >> ~/.bash_profile 
    ln -s /FrameworkBenchmarks ~/FrameworkBenchmarks
    if [ -e "/FrameworkBenchmarks/benchmark.cfg" ]; then
      echo "You have a benchmark.cfg file that will interfere with Vagrant, moving"
      mv /FrameworkBenchmarks/benchmark.cfg /FrameworkBenchmarks/benchmark.cfg.backup
    fi
  else
    echo "export FWROOT=/home/vagrant/FrameworkBenchmarks" >> ~/.bash_profile 
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
  if [ "$1" != "dev" ]; then
    git clone https://github.com/TechEmpower/FrameworkBenchmarks.git $FWROOT
  fi
  sudo pip install -r $FWROOT/config/python_requirements.txt

  # Setup databases, client workload generator
  cd $FWROOT
  toolset/run-tests.py --verbose --install all --test ''

  # Setup a nice welcome message for our guest
  sudo rm /etc/update-motd.d/51-cloudguest
  sudo rm /etc/update-motd.d/98-cloudguest
  sudo cp /vagrant/custom_motd.sh /etc/update-motd.d/55-tfbwelcome

cat | sudo tee /etc/update-motd.d/05-tfbwelcome <<EOF
  Welcome to the FrameworkBenchmarks project!

  To get started, perhaps try this: 
    $ cd FrameworkBenchmarks
    $ toolset/run-tests.py --install server --test go
    $ cat results/ec2/latest/logs/go/out.txt

  You can get tons of help with this: 
    $ toolset/run-tests.py --install server --test go

  This environment is already setup and ready to go, so you 
  can safely ignore any flags about users, hosts, or identity files. 
EOF


  touch ~/.firstboot

fi

