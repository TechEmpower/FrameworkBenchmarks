#!/bin/bash

fw_depends swift3 mongodb mysql postgresql

sudo apt-get install -y libmysqlclient-dev

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-postgresql --env=production &
