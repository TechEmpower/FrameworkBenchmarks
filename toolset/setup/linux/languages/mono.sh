#!/bin/bash

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

# what do we want? latest mono
# how do we want it? already compiled from packages but without sudo

# save environment
cat > $IROOT/mono.installed <<'END'
export SNAPDATE=20141220092712
export MONO_HOME=$IROOT/mono-snapshot-$SNAPDATE
export MONO_PATH=$MONO_HOME/lib/mono/4.5
export MONO_CFG_DIR=$MONO_HOME/etc
export PATH=$MONO_HOME/bin:$PATH
export LD_LIBRARY_PATH=$MONO_HOME/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$MONO_HOME/lib/pkgconfig:$PKG_CONFIG_PATH
END

# load environment
. $IROOT/mono.installed

# temp dir for extracting archives
TEMP=$IROOT/mono-snapshot-${SNAPDATE}-temp

# start fresh
rm -rf $TEMP && mkdir -p $TEMP
rm -rf $MONO_HOME && mkdir -p $MONO_HOME

# download .debs and extract them into $TEMP dir
fw_get http://jenkins.mono-project.com/repo/debian/pool/main/m/mono-snapshot-${SNAPDATE}/mono-snapshot-${SNAPDATE}_${SNAPDATE}-1_amd64.deb
fw_get http://jenkins.mono-project.com/repo/debian/pool/main/m/mono-snapshot-${SNAPDATE}/mono-snapshot-${SNAPDATE}-assemblies_${SNAPDATE}-1_all.deb
dpkg-deb -x mono-*amd64.deb $TEMP
dpkg-deb -x mono-*assemblies*.deb $TEMP

# move /opt/mono-$SNAPDATE to /installs
mv $TEMP/opt/mono-*/* $MONO_HOME

# cleanup
rm mono-*.deb
rm -rf $TEMP

# replace /opt/mono-$SNAPDATE path
file $MONO_HOME/bin/* | grep "POSIX shell script" | awk -F: '{print $1}' | xargs sed -i "s|/opt/mono-$SNAPDATE|$MONO_HOME|g"
sed -i "s|/opt/mono-$SNAPDATE|$MONO_HOME|g" $MONO_HOME/lib/pkgconfig/*.pc $MONO_HOME/etc/mono/config

# import SSL certificates
#mozroots --import --sync
echo -e 'y\ny\ny\n' | certmgr -ssl https://nuget.org

touch $IROOT/mono.installed
