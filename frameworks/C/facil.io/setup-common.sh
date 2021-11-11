#!/bin/bash

# Don't redownload facil.io unless using the FIO_EDGE flag.
if [[ (! -d facil_app) ||  (-n "${FIO_EDGE}") ]] ; then
	# remove existing installation, if any
	if [ -e facil_app ] ; then
		rm -R facil_app
	fi

	# create new installation folder
	mkdir facil_app
	cd facil_app

	#### Download and unpack

	# Download source selection
	# Setting FIO_EDGE will test against the master branch on the development machine. i.e.:
	#     $ FIO_EDGE=1 tfb --mode verify --test facil.io
	if [[ -z "${FIO_EDGE}" ]]; then
		echo "INFO: loading facil.io version 0.7.0.beta7"
	  FIO_URL="https://api.github.com/repos/boazsegev/facil.io/tarball/0.7.0.beta7"
	else
		echo "INFO: development mode detected, loading facil.io from master."
		FIO_URL="https://github.com/boazsegev/facil.io/archive/master.tar.gz"
	fi
	# Download
	curl -s -o facil.io.tar.gz -LJO $FIO_URL
	# Unpack
	tar --strip-components=1 -xzf facil.io.tar.gz
	if [ $? -ne 0 ]; then echo "Couldn't extract tar."; exit 1; fi
	# Cleanup
	rm facil.io.tar.gz
	./scripts/new/cleanup
	cd ..
fi

# remove any existing source files, such as boiler plate
rm -R facil_app/src
mkdir facil_app/src
