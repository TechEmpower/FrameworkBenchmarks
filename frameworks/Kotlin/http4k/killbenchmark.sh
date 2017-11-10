#!/usr/bin/env bash
ps -ef | grep benchmark | awk '{print $2}' | xargs kill
