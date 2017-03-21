#!/bin/bash

fw_depends swift3 mongodb mysql postgresql

swift build -Xswiftc -DNOJSON -c release

.build/release/vapor-tfb-postgresql --env=production &
