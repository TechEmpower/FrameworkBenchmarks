#!/bin/bash

fw_depends java maven

./setup.sh -Dsabina.backend=undertow -Dsabina.benchmark.repository=mysql
