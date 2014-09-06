#!/bin/bash

sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install oracle-java8-installer

# set Java 7 (OpenJDK) as the default, as previously.
sudo update-alternatives --set java /usr/lib/jvm/java-7-openjdk-amd64/jre/bin/java
sudo update-alternatives --set javac /usr/lib/jvm/java-7-openjdk-amd64/bin/javac

update-alternatives --get-selections | grep java-8 | sed -e 's/java-8-oracle/java-7-openjdk-amd64/' | while read line
do
  l=(${line// / })
  n=${l[0]}
  p=${l[2]}
  if [ -e "$p" ]; then
    sudo update-alternatives --set $n "$p"
  fi
done
