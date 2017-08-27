
#!/bin/bash

fw_depends java postgresql

./gradlew -x test
export DBSTORE='postgresql'
export WEBENGINE='undertow'
nohup build/install/hexagon/bin/hexagon &
