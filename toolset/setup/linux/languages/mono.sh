#!/bin/bash

post_install () {
  . mono-snapshot mono/20141222114925
  
  echo "Installing SSL certificates"
  sudo env "PATH=$PATH" mozroots --import --sync --machine
  echo -e 'y\ny\ny\n' | sudo env "PATH=$PATH" certmgr -ssl -m https://nuget.org

  # For apps that need write access to the registry
  sudo mkdir -p /etc/mono/registry
  sudo chmod 777 /etc/mono/registry
}

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { 
  post_install
  return 0
}

echo "Installing mono from CI packages"
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://jenkins.mono-project.com/repo/debian sid main" | sudo tee /etc/apt/sources.list.d/mono-jenkins.list
sudo apt-get update
sudo apt-get install -y mono-snapshot-20141222114925

post_install

touch $IROOT/mono.installed
