#!/bin/bash

set -ex

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Installing RootCAs from Mozilla..."; 
  # Load environment variables
  . $IROOT/mono.installed
  mozroots --import --sync;
  return 0; }

# what do we want? latest mono
# how do we want it? already compiled from packages but without sudo apt-get
# See https://github.com/TechEmpower/FrameworkBenchmarks/pull/1287

# Add source for prepackaged binaries
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://jenkins.mono-project.com/repo/debian sid main" | sudo tee /etc/apt/sources.list.d/mono-jenkins.list
sudo apt-get update

# Find the most recent snapshot
SNAPSHOT=$(apt-cache search 'mono-snapshot-.*-assemblies' | cut -d'-' -f3 | tail -1)

# save environment

echo "export SNAPDATE=$SNAPSHOT" > $IROOT/mono.installing
cat >> $IROOT/mono.installing <<'END'
export MONO_HOME=$IROOT/mono-snapshot-$SNAPDATE
export MONO_PATH=$MONO_HOME/lib/mono/4.5
export MONO_CFG_DIR=$MONO_HOME/etc
export PATH=$MONO_HOME/bin:$PATH
export LD_LIBRARY_PATH=$MONO_HOME/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$MONO_HOME/lib/pkgconfig:$PKG_CONFIG_PATH
END

# load environment
. $IROOT/mono.installing

# start fresh
rm -rf $MONO_HOME && mkdir -p $MONO_HOME

# Download and extract debs
fw_apt_to_iroot mono-snapshot-$SNAPSHOT
fw_apt_to_iroot mono-snapshot-${SNAPSHOT}-assemblies mono-snapshot-$SNAPSHOT

# Simplify paths
mv $MONO_HOME/opt/mono-*/* $MONO_HOME
file $MONO_HOME/bin/* | grep "POSIX shell script" | awk -F: '{print $1}' | xargs sed -i "s|/opt/mono-$SNAPDATE|$MONO_HOME|g"
sed -i "s|/opt/mono-$SNAPDATE|$MONO_HOME|g" $MONO_HOME/lib/pkgconfig/*.pc $MONO_HOME/etc/mono/config

# import SSL certificates
mozroots --import --sync
#echo -e 'y\ny\ny\n' | certmgr -ssl https://nuget.org

mv $IROOT/mono.installing $IROOT/mono.installed
