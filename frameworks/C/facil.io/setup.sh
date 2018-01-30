#!/bin/bash

# enter root folder
cd $TROOT

if ! [ -d facil_app ] ; then
	mkdir facil_app
	cd facil_app
	curl -s -o facil.io.tar.gz -LJO https://api.github.com/repos/boazsegev/facil.io/tarball/v.0.6.0.beta
	tar --strip-components=1 -xzf facil.io.tar.gz
	if [ $? -ne 0 ]; then echo "Couldn't extract tar."; exit 1; fi
	rm facil.io.tar.gz
	./scripts/new/cleanup
	cd ..
fi

# recompile test
rm -r facil_app/src
mkdir facil_app/src
cp bench_app.c facil_app/src
cd facil_app
make -j build

# run test
cd tmp
./demo -b 0.0.0.0 -p 8080 -db "TFB-database" -w -1 -t 1 &
# step out
cd ../..
