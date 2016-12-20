#!/usr/bin/env bash

export DEBIAN_FRONTEND=noninteractive

# Set up a new PATH variable
export PATH=/home/travis/bin:/home/travis/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# Remove services/installs that travis gives us
sudo apt-get purge ruby* -yqq

# Travis adds sources for the extra services they provide. When doing
# apt updates this can interfere with the standard ubuntu installs so we'll reset the sources.
sudo mv ./toolset/travis/sources.list /etc/apt/sources.list
sudo rm -rf /etc/apt/sources.list.d
sudo mkdir /etc/apt/sources.list.d

# Postgres user permission fix for travis
sudo chmod og+rX /home /home/travis

# Remove pre-installed rmv
rvm implode --force
sudo rm -rf /etc/rvmrc
sudo rm ~/.rvmrc
