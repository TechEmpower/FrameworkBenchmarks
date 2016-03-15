#!/bin/bash

fw_depends crystal

crystal deps install

crystal build --release server-postgres.cr

./server-postgres -e production &
