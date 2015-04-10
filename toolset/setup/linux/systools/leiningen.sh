#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/lein.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

mkdir -p lein/bin
fw_get https://raw.github.com/technomancy/leiningen/stable/bin/lein -O leinbin
mv leinbin lein/bin/lein
chmod +x lein/bin/lein

echo "export LEIN_HOME=$IROOT/lein" > $IROOT/lein.installed
echo "export PATH=$PATH:$IROOT/lein/bin" >> $IROOT/lein.installed
