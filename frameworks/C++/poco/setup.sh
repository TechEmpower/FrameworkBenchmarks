#!/bin/bash

fw_depends poco

g++-4.8 -O3 -DNDEBUG -std=c++0x -o poco benchmark.cpp -I$POCO_HOME/Foundation/include -I$POCO_HOME/Util/include -I$POCO_HOME/Net/include -L$POCO_HOME/lib/Linux/x86_64 -lPocoNet -lPocoUtil -lPocoFoundation -lPocoXML -lPocoJSON
./poco 8080 $MAX_THREADS

