#!/bin/bash

fw_depends java7 sbt

sed -i 's|> ".*:3306|> "'"${DBHOST}"':3306|g' src/main/scala/Main.scala

sbt update assembly

./run &
