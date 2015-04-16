#!/bin/bash

sed -i 's|host: \x27.*\x27|host: \x27'"${DBHOST}"'\x27|g' app.js

fw_depends java7 vertx 

vertx run app.js &
