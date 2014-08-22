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
  echo "export FWROOT=/home/vagrant/FrameworkBenchmarks" >> ~/.bash_profile 
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
  git clone https://github.com/TechEmpower/FrameworkBenchmarks.git $FWROOT
  sudo pip install -r $FWROOT/config/python_requirements.txt

  # Setup databases, client workload generator
  cd $FWROOT
  toolset/run-tests.py --verbose --install all --test ''

  # Setup a nice welcome message for our guest
  rm /etc/motd
cat | sudo tee /etc/motd <<EOF
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

