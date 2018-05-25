#!/bin/bash

# Propagate any failure
set -e

# Install Java8 manually (fw_depends java8 is failing in vagrant-development)
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get install -y oracle-java8-installer

# Ensure maven uses appropriate version of Java
export JAVA_HOME=/usr/lib/jvm/java-8-oracle/
sudo update-alternatives --set java /usr/lib/jvm/java-8-oracle/jre/bin/java
sudo update-alternatives --set javac /usr/lib/jvm/java-8-oracle/bin/javac

# Need maven 3.1.1 or higher (default for Ubuntu is 3.0.5)
echo "Loading maven ..."
sudo add-apt-repository "deb http://ppa.launchpad.net/natecarlson/maven3/ubuntu precise main"
sudo apt-get update
sudo apt-get -y --force-yes install maven3
if [ -e /usr/bin/mvn ]
then
    sudo rm -f /usr/bin/mvn
fi
sudo ln -s /usr/share/maven3/bin/mvn /usr/bin/mvn

# Setup configuration file (normally properties files contains environment specific information but create copy to avoid SCM issues)
echo "Creating configuration for OfficeFloor environment ..."
mkdir -p ./production
cp ./raw/datasource.properties ./production 
sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' ./production/datasource.properties
echo "Configuration created"

# Compile application
echo "Building OfficeFloor test application ..."
mvn -DskipTests clean package
echo "OfficeFloor test application built"

# Run application
echo "Starting OfficeFloor application"
mvn -DincludeGWT=false -DenvDir=production net.officefloor.maven:woof-maven-plugin:run