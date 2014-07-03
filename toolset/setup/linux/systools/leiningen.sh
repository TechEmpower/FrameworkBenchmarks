#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists bin/lein
[ $? -ne 0 ] || { echo "Leiningen is installed!"; return 0; }

mkdir -p bin
fw_get https://raw.github.com/technomancy/leiningen/stable/bin/lein
mv lein bin/lein
chmod +x bin/lein
