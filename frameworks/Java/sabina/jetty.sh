#!/bin/bash

fw_depends java maven

./setup.sh -Dsabina.backend=jetty -Dsabina.benchmark.repository=mysql
