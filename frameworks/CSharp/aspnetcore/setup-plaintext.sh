#!/bin/bash

if [ "$(nproc)" -eq "80" ]
then
    threadCount=24
fi

if [ "$(nproc)" -eq "4" ]
then
    threadCount=2
fi

if [ -z "$threadCount" ]
then
    echo "Invalid thread count ($(nproc)), using default"
    threadCount=2
fi

source run-linux.sh plaintext $threadCount
