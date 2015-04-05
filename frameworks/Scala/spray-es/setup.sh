#!/bin/bash

/opt/java8/bin/java -Dtfb.db_host=$DBHOST -jar target/scala-2.11/spray-es-assembly-0.1.jar &
