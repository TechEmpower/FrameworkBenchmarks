#!/bin/bash

fw_exists bin/lein
[ $? -ne 0 ] || { return 0; }

mkdir -p bin
fw_get https://raw.github.com/technomancy/leiningen/stable/bin/lein
mv lein bin/lein
chmod +x bin/lein
