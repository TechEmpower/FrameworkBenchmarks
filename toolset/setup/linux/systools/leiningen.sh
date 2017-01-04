#!/bin/bash

fw_installed lein && return 0

mkdir -p lein/bin
fw_get -o leinbin https://raw.github.com/technomancy/leiningen/stable/bin/lein
mv leinbin lein/bin/lein
chmod +x lein/bin/lein

LEIN_HOME=$IROOT/lein
echo "export LEIN_HOME=${LEIN_HOME}" > $IROOT/lein.installed
echo -e "export PATH=\$LEIN_HOME/bin:\$PATH" >> $IROOT/lein.installed

source $IROOT/lein.installed
