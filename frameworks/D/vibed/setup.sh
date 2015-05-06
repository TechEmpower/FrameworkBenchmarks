#!/bin/bash
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' source/app.d

dub --build=release &