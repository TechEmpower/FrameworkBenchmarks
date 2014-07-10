#!/bin/bash

# TODO include a check before we do all this, because it's 
# annoyingly slow to run apt-get if we don't need to
sudo apt-get install -y openjdk-7-jdk
sudo apt-get remove -y --purge openjdk-6-jre openjdk-6-jre-headless