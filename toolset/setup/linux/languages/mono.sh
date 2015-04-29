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
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://jenkins.mono-project.com/repo/debian sid main" | sudo tee /etc/apt/sources.list.d/mono-jenkins.list
sudo apt-get update

# Find the most recent snapshot
#SNAPSHOT=$(apt-cache search 'mono-snapshot-.*-assemblies' | cut -d'-' -f3 | tail -1)
SNAPSHOT="20150202010831"

# save environment

MONO_HOME=$IROOT/mono-snapshot-$SNAPSHOT
echo "export SNAPSHOT=$SNAPSHOT" > $IROOT/mono.installing
echo "export MONO_HOME=$MONO_HOME" >> $IROOT/mono.installing
echo "export MONO_PATH=$MONO_HOME/lib/mono/4.5" >> $IROOT/mono.installing
echo "export MONO_CFG_DIR=$MONO_HOME/etc" >> $IROOT/mono.installing
echo -e "export PATH=$MONO_HOME/bin:\$PATH" >> $IROOT/mono.installing
echo -e "export LD_LIBRARY_PATH=$MONO_HOME/lib:\$LD_LIBRARY_PATH" >> $IROOT/mono.installing
echo -e "export PKG_CONFIG_PATH=$MONO_HOME/lib/pkgconfig:\$PKG_CONFIG_PATH" >> $IROOT/mono.installing

# load environment
source $IROOT/mono.installing

# start fresh
rm -rf $MONO_HOME && mkdir -p $MONO_HOME

# Download and extract debs
fw_apt_to_iroot mono-snapshot-$SNAPSHOT
fw_apt_to_iroot mono-snapshot-$SNAPSHOT-assemblies mono-snapshot-$SNAPSHOT

# Simplify paths
mv $MONO_HOME/opt/mono-*/* $MONO_HOME
file $MONO_HOME/bin/* | grep "POSIX shell script" | awk -F: '{print $1}' | xargs sed -i "s|/opt/mono-$SNAPSHOT|$MONO_HOME|g"
sed -i "s|/opt/mono-$SNAPSHOT|$MONO_HOME|g" $MONO_HOME/lib/pkgconfig/*.pc $MONO_HOME/etc/mono/config

echo "mozroots --import --sync" >> $IROOT/mono.installing

mv $IROOT/mono.installing $IROOT/mono.installed

source $IROOT/mono.installed
