
#!/bin/bash

fw_depends java mongodb

./gradlew -x test
export DBSTORE='mongodb'
export WEBENGINE='resin'

rm -rf $RESIN_HOME/webapps/*
cp build/libs/ROOT.war $RESIN_HOME/webapps
resinctl start

