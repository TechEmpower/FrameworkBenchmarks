#!/bin/bash

set -x
export DEBIAN_FRONTEND=noninteractive

# One -q produces output suitable for logging (mostly hides
# progress indicators)
sudo apt-get -yq update

# WARNING: DONT PUT A SPACE AFTER ANY BACKSLASH OR APT WILL BREAK
sudo apt-get -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
  git-core \
  cloc dstat                    `# Collect resource usage statistics` \
  python-dev \
  python-pip \
  python-software-properties \
  libmysqlclient-dev            `# Needed for MySQL-python` \
  libpq-dev                     `# Needed for psycopg2` \
  linux-image-extra-$(uname -r) `# Needed for Docker on Ubuntu 14` \
  linux-image-extra-virtual     `# Needed for Docker on Ubuntu 14` \
  apt-transport-https           `# Needed for Docker on Ubuntu 14` \
  ca-certificates               `# Needed for Docker on Ubuntu 14` \
  curl                          `# Needed for Docker on Ubuntu 14` \
  software-properties-common    `# Needed for Docker on Ubuntu 14`
  # cmake build-essential automake    `# Needed for building code` \
  # wget unzip                   `# Common tools` \
  # mercurial                `# Version control systems` \
  # libpcre3 libpcre3-dev libpcrecpp0 `# Regular expression support` \
  # libssl-dev libcurl4-openssl-dev   `# SSL libraries` \
  # zlib1g-dev \
  # libreadline6-dev \
  # libbz2-dev \
  # libyaml-dev libxml2-dev \
  # libxslt-dev libgdbm-dev ncurses-dev  \
  # libffi-dev htop libtool bison libevent-dev \
  # libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
  # liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev \
  # libjson0-dev libmcrypt-dev libicu-dev gettext \
  #  mlton \
  # re2c libnuma-dev

sudo pip install colorama==0.3.1
# Version 2.3 has a nice Counter() and other features
# but it requires --allow-external and --allow-unverified
sudo pip install progressbar==2.2 requests MySQL-python psycopg2 pymongo

#
# Install Rust
#
curl -sL https://sh.rustup.rs -o rustup.sh
chmod 777 rustup.sh
./rustup.sh -y
rm ./rustup.sh
source ~/.profile
cd toolset/setup/linux/docker/TFBReaper
cargo build
cd ../../../../..

#
# Install Docker
#
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update
sudo apt-get -qqy install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold"  docker-ce
sudo groupadd docker
sudo usermod -aG docker $USER

sudo pip install docker==3.1.0

# Get the ulimit from the benchmark config
if [ -f benchmark.cfg ]; then
  FILE=benchmark.cfg
else
  FILE=benchmark.cfg.example
fi

ULIMIT=$(grep '^ulimit=' $FILE | grep -Po '[0-9]+')

if [ ! $ULIMIT ]; then ULIMIT=200000; fi;

sudo sh -c "echo '*               -    nofile          ${ULIMIT}' >> /etc/security/limits.conf"
sudo sh -c "echo '*            hard    rtprio             99' >> /etc/security/limits.conf"
sudo sh -c "echo '*            soft    rtprio             99' >> /etc/security/limits.conf"

# Create a tfb command alias for running the toolset
# For now, this still ensures you have to be in the framework root to run it
sudo tee /etc/profile.d/tfb.sh <<EOF
#!/bin/bash
tfb() {
  $(pwd)/toolset/run-tests.py "\$@"
}
EOF
source /etc/profile.d/tfb.sh