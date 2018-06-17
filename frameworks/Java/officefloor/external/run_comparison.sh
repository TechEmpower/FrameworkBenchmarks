#!/bin/bash

# Find tfb directory
while [ ! -f tfb ]; do
	cd ..
done

# Run comparision
./tfb --test h2o actix-raw rapidoid-http-fast vertx-postgres vertx-web-postgres
