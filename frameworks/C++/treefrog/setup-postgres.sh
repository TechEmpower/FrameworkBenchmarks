#!/bin/bash

sed -i 's|HostName=.*|HostName='"$DBHOST"'|g' config/database.ini
sed -i 's|HostName=.*|HostName='"$DBHOST"'|g' config/mongodb.ini
sed -i 's|DriverType=.*|DriverType=QPSQL|g' config/database.ini
sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=thread|g' config/application.ini

fw_depends treefrog

# 1. Generate Makefile
qmake -r CONFIG+=release

# 2. Compile applicaton
make

# 3. Clean log files
rm -f log/*.log

# 4. Start TreeFrog
treefrog -d $TROOT
