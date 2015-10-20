#!/bin/bash

fw_depends java8 maven

./setup.sh -Dsabina.backend=jetty -Dsabina.benchmark.repository=mysql
