#!/bin/bash

sed -i 's|host: \x27.*\x27|host: \x27'"${DBHOST}"'\x27|g' app.js

${IROOT}/vert.x-2.1.1/bin/vertx run app.js &