#!/bin/bash

fw_depends swift3 ctls cmysql mongodb mysql postgresql

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-mysql --env=production &
