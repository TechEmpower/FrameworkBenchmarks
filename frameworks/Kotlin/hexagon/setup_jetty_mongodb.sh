
#!/bin/bash

fw_depends java mongodb

gradle/wrapper -x test

nohup build/install/hexagon/bin/hexagon mongodb &
#rm -rf $RESIN_HOME/webapps/*
#cp build/libs/ROOT.war $RESIN_HOME/webapps
#resinctl start
