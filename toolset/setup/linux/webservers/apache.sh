#!/bin/bash

# TODO: Get away from apt-get
# TODO2: Does anything use apache2???
sudo apt-get install -y apache2

# Stop Apache; it starts after it is installed
sudo /etc/init.d/apache2 stop
