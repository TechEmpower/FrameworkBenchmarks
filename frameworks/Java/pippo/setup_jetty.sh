#!/bin/bash

fw_depends java maven

mvn clean package

java -Xms2G -Xmx2G -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -cp target/pippo-benchmark-0.1.0.jar ro.pippo.benchmark.BenchmarkJetty &