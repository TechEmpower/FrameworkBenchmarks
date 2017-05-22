
#!/bin/bash

fw_depends java mongodb

gradle/wrapper -x test


rm -rf $RESIN_HOME/webapps/*
cp build/libs/ROOT.war $RESIN_HOME/webapps
resinctl start

