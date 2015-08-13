#!/bin/bash

# Setup configuration file (normally properties files contains environment specific information but create copy to avoid SCM issues)
mkdir -p ./production
cp ./raw/datasource.properties ./production 
sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' ./production/datasource.properties

# Compile and run application
mvn -DskipTests clean compile
mvn -Denvironment.properties.directory=production net.officefloor.maven:woof:run
