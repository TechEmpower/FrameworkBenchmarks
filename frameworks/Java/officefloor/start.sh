#!/bin/bash

# Propagate any failure
set -e

# Setup configuration file (normally properties files contains environment specific information but create copy to avoid SCM issues)
echo "Creating configuration for OfficeFloor environment ..."
mkdir -p ./production
cp ./raw/datasource.properties ./production 
sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' ./production/datasource.properties
echo "Configuration created"

# Compile application
echo "Building OfficeFloor test application ..."
mvn -DskipTests clean install
echo "OfficeFloor test application built"

# Run application
echo "Starting OfficeFloor application"
mvn -DenvDir=production net.officefloor.maven:maven-woof-plugin:run
