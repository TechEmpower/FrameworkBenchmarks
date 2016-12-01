#!/usr/bin/env bash

cp ../../../config/create.sql .

docker build -t dbi .

rm create.sql

docker run --name db -d \
  -e MYSQL_ROOT_PASSWORD=123 \
  -p 3306:3306 dbi
