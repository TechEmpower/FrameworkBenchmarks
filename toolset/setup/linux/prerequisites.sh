#!/bin/bash

RETCODE=$(fw_exists fwbm_prereqs_installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Prerequisites installed!"; 
  return 0; 
}

sudo apt-get -y update
sudo apt-get -y upgrade
sudo apt-get -y install build-essential \
  libpcre3 libpcre3-dev libpcrecpp0 \
  libssl-dev zlib1g-dev python-software-properties \
  unzip git-core libcurl4-openssl-dev libbz2-dev \
  libmysqlclient-dev mongodb-clients libreadline6-dev \
  libyaml-dev libsqlite3-dev sqlite3 libxml2-dev \
  libxslt-dev libgdbm-dev ncurses-dev automake \
  libffi-dev htop libtool bison libevent-dev \
  libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
  liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev \
  libjson0-dev libmcrypt-dev libicu-dev cmake gettext \
  curl libpq-dev mercurial mlton cloc wget dstat

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo apt-get -y update
sudo apt-get install -y gcc-4.8 g++-4.8

# Stop permanently overwriting people's files just for 
# trying out our software!
RETCODE=$(fw_exists ~/.bash_profile.bak)
[ ! "$RETCODE" == 0 ] || { \
  echo "Backing up your original ~/.bash_profile, ~/.profile, ~/.bashrc"
  mv ~/.bash_profile ~/.bash_profile.bak || true
  mv ~/.profile ~/.profile.bak || true
  mv ~/.bashrc ~/.bashrc.bak || true
}

cp ../config/benchmark_profile ~/.bash_profile
cat ../config/benchmark_profile >> ~/.profile
cat ../config/benchmark_profile >> ~/.bashrc
sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

touch fwbm_prereqs_installed
