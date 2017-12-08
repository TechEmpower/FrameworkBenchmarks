#!/bin/bash

fw_depends postgresql mysql java maven
 

mvn -U clean package
cd target
java -Dlogback.configurationFile="conf/logback.xml" -Dconfig.file="conf/application.conf" -server  -Xms1g -Xmx2g -classpath "./proteus-techempower-1.0.0.jar:lib/*" io.sinistral.ExampleApplication 
