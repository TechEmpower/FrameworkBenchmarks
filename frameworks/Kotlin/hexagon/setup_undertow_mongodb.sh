
#!/bin/bash

fw_depends java mongodb

./gradlew -x test
export DBSTORE='mongodb'
export WEBENGINE='undertow'
nohup build/install/hexagon/bin/hexagon &
