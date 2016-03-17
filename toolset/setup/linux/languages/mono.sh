#!/bin/bash

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/mono.installed
  return 0; }

# what do we want? latest mono
# how do we want it? already compiled from packages but without sudo apt-get
# See https://github.com/TechEmpower/FrameworkBenchmarks/pull/1287

# Add source for prepackaged binaries
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
sudo apt-get update

# Ideally we would install specific version of Mono, ie, mono-complete=4.2.2.30-0xamarin1 but it seems that too doesn't stick around for too long ;(
# So we are back to square one with "unsable" Mono, although Mono is much more stable than it was at version 3
sudo apt-get install -y mono-complete

# Mono installs to PATH (/usr/bin/mono) :( so no env vars

echo "# Mono installed to PATH /usr/bin " > $IROOT/mono.installed

source $IROOT/mono.installed
