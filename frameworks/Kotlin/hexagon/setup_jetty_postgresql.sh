
#!/bin/bash

fw_depends java postgresql

./gradlew -x test
export DBSTORE='postgresql'
nohup build/install/hexagon/bin/hexagon &
