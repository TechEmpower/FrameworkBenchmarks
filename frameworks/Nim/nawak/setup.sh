#!/bin/bash

sed -i 's|host=127.0.0.1 port=5432|host='"${DBHOST}"' port=5432|g' model_postgre.nim

fw_depends nim nimble zeromq mongrel2 nawak

echo "** Compiling app"
nim c --threads:on -d:release -d:postgre_model -l:-Wl,-rpath,$IROOT/zeromq-4.0.3/lib -o:nawak_postgre app.nim

echo "** Starting mongrel2"
cd conf
mkdir -p run logs tmp
m2sh load -config mongrel2.conf
m2sh start -name test &

cd ..
nb_workers=256
if [ "$TRAVIS" == "1" ]; then
  nb_workers=32
fi

echo "** Starting app"

./nawak_postgre $nb_workers &
