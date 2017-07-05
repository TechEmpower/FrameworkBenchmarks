
#!/bin/bash

fw_depends java mongodb

gradle/wrapper -x test
export DBSTORE='mongodb'
nohup build/install/hexagon/bin/hexagon &
