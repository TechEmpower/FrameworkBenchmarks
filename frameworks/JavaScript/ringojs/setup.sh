#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' ringo-main.js

rp install --force-yes

ringo --production -J-server -J-Xmx1g -J-Xms1g --modules ./packages/ ringo-main.js &