#!/bin/bash

RETCODE=$(fw_exists java8.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install -y oracle-java8-installer

sudo ln -sf /usr/lib/jvm/java-8-oracle /opt/java8

# set Java 7 (OpenJDK) as the default, as before.
sudo update-alternatives --set java /usr/lib/jvm/java-7-openjdk-amd64/jre/bin/java
sudo update-alternatives --set javac /usr/lib/jvm/java-7-openjdk-amd64/bin/javac

# try to make sure all JDK binaries default to OpenJDK 7, not Java 8.
update-alternatives --get-selections | grep java-8 | sed -e 's/java-8-oracle/java-7-openjdk-amd64/' | while read line
do
  l=(${line// / })
  n=${l[0]}
  p=${l[2]}
  if [ -e "$p" ]; then
    sudo update-alternatives --set $n "$p"
  fi
done

touch $IROOT/java8.installed
