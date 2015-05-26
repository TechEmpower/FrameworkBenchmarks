#!/bin/bash

crystal build --release server.cr -o server.out
./server.out
