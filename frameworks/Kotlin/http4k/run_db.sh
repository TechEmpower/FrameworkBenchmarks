#!/usr/bin/env bash

docker rm -fv $(docker ps -aq)
rm -rf sql
mkdir -p sql
cp -r ./../../../toolset/setup/linux/databases/postgresql/create-postgres.sql ./sql/
docker build -t dbi .

docker run  -p 5432:5432 --name db -d dbi

function identity {
	docker ps -a| grep $1 | awk '{print $1}'  | grep -v 'CONTAINER'
}

export name=`identity dbi`
docker logs -f ${name}