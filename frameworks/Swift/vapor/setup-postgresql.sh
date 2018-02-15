#!/bin/bash

fw_depends swift ctls cmysql postgresql

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-postgresql --env=production &
