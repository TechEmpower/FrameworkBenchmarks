#!/bin/bash

fw_depends luna

CC=gcc-4.9 CXX=g++-4.9 conan install --build=missing -s compiler="gcc" -s compiler.version="4.9" .
cmake . -DCMAKE_CXX_COMPILER=g++-4.9 -DCMAKE_CC_COMPILER=gcc-4.9
cmake --build .

$TROOT/bin/lunabench_thread 8080
