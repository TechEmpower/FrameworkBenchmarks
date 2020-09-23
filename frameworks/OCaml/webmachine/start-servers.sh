#!/bin/bash

CPU_COUNT=$(nproc)
P=9000
END=$(($P+$CPU_COUNT))

while [ $P -lt $END ]; do
  PORT=$P /webmachine/_build/default/tfb.exe &
  let P=P+1
done
