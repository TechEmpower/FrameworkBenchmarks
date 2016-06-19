#!/bin/bash

sed -i 's|HostName=.*|HostName='"$DBHOST"'|g' config/database.ini
sed -i 's|HostName=.*|HostName='"$DBHOST"'|g' config/mongodb.ini
sed -i 's|DriverType=.*|DriverType=QMYSQL|g' config/database.ini
sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=hybrid|g' config/application.ini

fw_depends treefrog

# 1. Generate Makefile
qmake -r CONFIG+=release

# 2. Compile applicaton
make

# 3. Clean log files
rm -f log/*.log

# 4. Start TreeFrog
treefrog -d $TROOT
