#!/bin/bash

fw_depends swift ctls cmysql mysql

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-mysql --env=production &
