#!/bin/bash

CPU_COUNT=$(nproc)
P=3000
END=$(($P+$CPU_COUNT))
CONF=""

while [ $P -lt $END ]; do
  CONF+="\t\tserver 127.0.0.1:$P;\n"
  let P=P+1
done

sed -i "s|# replace|$CONF|g" nginx.conf
