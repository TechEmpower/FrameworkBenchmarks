
#!/bin/bash

fw_depends java mongodb

./gradlew -x test
export DBSTORE='mongodb'
export WEBENGINE='jetty'
nohup build/install/hexagon/bin/hexagon &
