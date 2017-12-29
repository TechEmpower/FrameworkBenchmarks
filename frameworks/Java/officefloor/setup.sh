#!/bin/bash

# Propagate any failure
set -e

# Setup java and maven
fw_depends mysql java maven

# Setup configuration file (normally properties files contains environment specific information but create copy to avoid SCM issues)
echo "Creating configuration for OfficeFloor environment ..."
mkdir -p ./production
cp ./raw/datasource.properties ./production 
echo "Configuration created"

# Compile application
echo "Building OfficeFloor test application ..."
mvn -DskipTests clean package
echo "OfficeFloor test application built"

# Run application
echo "Starting OfficeFloor application"
mvn -e -X -DincludeGWT=false -DenvDir=production net.officefloor.maven:woof-maven-plugin:run
