#!/bin/bash

set -x
export DEBIAN_FRONTEND=noninteractive

RETCODE=$(fw_exists fwbm_prereqs_installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Prerequisites installed!"; 
  return 0; 
}


# Use a more recent version of Mongo shell
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list

sudo apt-get -y update
sudo apt-get -y upgrade -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold"

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
sudo apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  cmake build-essential automake    `# Needed for building code` \
  curl wget unzip                   `# Common tools` \
  software-properties-common        `# Needed for add-apt-repository` \
  git-core mercurial                `# Version control systems` \
  libpcre3 libpcre3-dev libpcrecpp0 `# Regular expression support` \
  libssl-dev libcurl4-openssl-dev   `# SSL libraries` \
  libmysqlclient-dev \
  mongodb-org-shell \
  libsqlite3-dev sqlite3            `# Database libraries` \
  zlib1g-dev python-software-properties \
  libreadline6-dev \
  libbz2-dev \
  libyaml-dev libxml2-dev \
  libxslt-dev libgdbm-dev ncurses-dev  \
  libffi-dev htop libtool bison libevent-dev \
  libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
  liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev \
  libjson0-dev libmcrypt-dev libicu-dev gettext \
  libpq-dev mlton \
  cloc dstat                        `# Collect resource usage statistics`

# Install gcc-4.8
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

sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

touch fwbm_prereqs_installed
