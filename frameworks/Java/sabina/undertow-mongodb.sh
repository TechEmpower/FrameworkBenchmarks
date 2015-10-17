#!/bin/bash

fw_depends java8 maven

./setup.sh -Dsabina.backend=undertow -Dsabina.benchmark.repository=mongodb
