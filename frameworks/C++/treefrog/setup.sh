#!/bin/bash

sed -i 's|HostName=.*|HostName='"$DBHOST"'|g' config/database.ini
sed -i "s|MultiProcessingModule=.*|MultiProcessingModule=hybrid|g" config/application.ini

# 1. Generate Makefile
qmake -r CONFIG+=release

# 2. Compile applicaton
make clean
make -j8

# 3. Clean log files
rm -f log/*.log

# 4. Start TreeFrog
treefrog -d $TROOT &