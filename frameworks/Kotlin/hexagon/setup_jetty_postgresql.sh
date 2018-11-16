
#!/bin/bash

fw_depends java postgresql

./gradlew -x test
export WEBENGINE='jetty'
nohup build/install/hexagon/bin/hexagon &
