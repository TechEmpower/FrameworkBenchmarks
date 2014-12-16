#!/bin/bash

sed -i 's|host=.* port=5432|host='"${DBHOST}"' port=5432|g' model_postgre.nim

export PATH=$NIMROD_HOME/bin:$NAWAK_HOME/bin:$ZEROMQ_HOME/include:$NAWAK_PATH:$PATH

nim c --threads:on -d:release -d:postgre_model --path:$NAWAK_PATH -o:nawak_postgre app.nim

cd conf
mkdir -p run logs tmp
m2sh load -config mongrel2.conf
m2sh start -name test

cd ..
nb_workers=256
if [ "$TRAVIS" == "1" ]; then
  nb_workers=32
if

./nawak_postgre &