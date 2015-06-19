#!/bin/bash

fw_depends crystal

crystal build --release server.cr -o server.out

./server.out
