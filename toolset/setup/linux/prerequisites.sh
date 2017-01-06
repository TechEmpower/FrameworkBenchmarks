#!/bin/bash

set -x
export DEBIAN_FRONTEND=noninteractive

source $FWROOT/toolset/setup/linux/bash_functions.sh

RETCODE=$(fw_exists fwbm_prereqs_installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Prerequisites installed!"; 
  return 0; }

# One -q produces output suitable for logging (mostly hides
# progress indicators)
sudo apt-get -yq update

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
sudo apt-get -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  cmake build-essential automake    `# Needed for building code` \
  curl wget unzip                   `# Common tools` \
  software-properties-common        `# Needed for add-apt-repository` \
  git-core mercurial                `# Version control systems` \
  libpcre3 libpcre3-dev libpcrecpp0 `# Regular expression support` \
  libssl-dev libcurl4-openssl-dev   `# SSL libraries` \
  libmysqlclient-dev \
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
  cloc dstat                        `# Collect resource usage statistics` \
  python-pip

sudo pip install colorama==0.3.1
# Version 2.3 has a nice Counter() and other features
# but it requires —-allow-external and -—allow-unverified
sudo pip install progressbar==2.2
sudo pip install requests

# Stop permanently overwriting people's files just
# for trying out our software!
RETCODE=$(fw_exists ~/.bash_profile.bak)
[ ! "$RETCODE" == 0 ] || { \
  echo "Backing up your original ~/.bash_profile, ~/.profile, ~/.bashrc"
  mv ~/.bash_profile ~/.bash_profile.bak || true
  mv ~/.profile ~/.profile.bak || true
  mv ~/.bashrc ~/.bashrc.bak || true
}

sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"
sudo sh -c "echo '*            hard    rtprio             99' >> /etc/security/limits.conf"
sudo sh -c "echo '*            soft    rtprio             99' >> /etc/security/limits.conf"

# Create a tfb command alias for running the toolset
# For now, this still ensures you have to be in the framework root to run it
alias tfb="./toolset/run-tests.py"
echo 'alias tfb="./toolset/run-tests.py"' >> ~/.bashrc

# Sudo in case we don't have permissions on IROOT
sudo touch fwbm_prereqs_installed

# Ensure everyone can see the file
sudo chmod 775 fwbm_prereqs_installed
