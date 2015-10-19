#!/bin/bash

sed -i 's|host=.*)|host="'$DBHOST'")|g' model_redis.nim

fw_depends nim nimble zeromq mongrel2 nawak

echo "** Compiling app"
nim c --threads:on -d:release -d:redis_model -l:-Wl,-rpath,$IROOT/zeromq-4.0.3/lib -o:nawak_redis app.nim

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

./nawak_redis $nb_workers &
