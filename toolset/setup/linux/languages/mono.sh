#!/bin/bash

post_install () {
  echo "Installing SSL certificates"
  sudo mozroots --import --sync --machine
  echo -e 'y\ny\ny\n' | sudo certmgr -ssl -m https://nuget.org

  # For apps that need write access to the registry
  sudo mkdir -p /etc/mono/registry
  sudo chmod 777 /etc/mono/registry
}

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { 
  post_install
  return 0
}

echo "Installing mono from official Xamarin packages for Debian"
curl -s http://download.mono-project.com/repo/xamarin.gpg | sudo apt-key add -
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/xamarin.list
sudo apt-get update
sudo apt-get -y install mono-complete

post_install

touch $IROOT/mono.installed
