
#!/bin/bash

fw_depends java postgresql

./gradlew -x test
export DBSTORE='postgresql'
export WEBENGINE='resin'

rm -rf $RESIN_HOME/webapps/*
cp build/libs/ROOT.war $RESIN_HOME/webapps
resinctl start

