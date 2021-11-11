#!/bin/sh

CPU_COUNT=$(nproc)
P=9000
END=$(($P+$CPU_COUNT))

while [ $P -lt $END ]; do
  PORT=$P /web/_build/default/bin/main.exe &
  let P=P+1
done
