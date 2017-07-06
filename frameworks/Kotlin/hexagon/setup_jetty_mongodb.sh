
#!/bin/bash

fw_depends java mongodb

./gradlew -x test
export DBSTORE='mongodb'
nohup build/install/hexagon/bin/hexagon &
