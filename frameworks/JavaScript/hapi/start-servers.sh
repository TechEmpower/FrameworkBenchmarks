#!/bin/bash

CPU_COUNT=$(nproc)
P=3000
END=$(($P+$CPU_COUNT))

while [ $P -lt $END ]; do
  PORT=$P node create-server.js &
  let P=P+1
done
