#!/bin/bash

# Propagate any failure
set -e

# Ensure maven uses appropriate version of Java
export JAVA_HOME=/usr/lib/jvm/java-8-oracle/
sudo update-alternatives --set java /usr/lib/jvm/java-8-oracle/jre/bin/java
sudo update-alternatives --set javac /usr/lib/jvm/java-8-oracle/bin/javac

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
