#!/bin/bash

RETCODE=$(fw_exists java8.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/java8.installed
  return 0; }

# TODO: Someday get away from apt-get
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install -y oracle-java8-installer

# set Java 7 (OpenJDK) as the default, as before.
J7_HOME=/usr/lib/jvm/java-7-openjdk-`dpkg --print-architecture`
sudo update-alternatives --set java $J7_HOME/jre/bin/java
sudo update-alternatives --set javac $J7_HOME/bin/javac

# try to make sure all JDK binaries default to OpenJDK 7, not Java 8
update-alternatives --get-selections | grep java-8 | sed -e "s|java-8-oracle|$J7_HOME|" | while read line
do
  l=(${line// / })
  n=${l[0]}
  p=${l[2]}
  if [ -e "$p" ]; then
    sudo update-alternatives --set $n "$p"
  fi
done

# Setup environment variables
JAVA_HOME=/usr/lib/jvm/java-8-oracle
echo "export JAVA_HOME=${JAVA_HOME}" > $IROOT/java8.installed
echo -e "export PATH=\$JAVA_HOME/bin:\$PATH" >> $IROOT/java8.installed

source $IROOT/java8.installed
