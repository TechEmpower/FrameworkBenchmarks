#!/bin/bash

fw_depends swift ctls cmysql mongodb

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-mongodb --env=production &
