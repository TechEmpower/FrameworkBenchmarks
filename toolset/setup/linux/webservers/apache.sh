#!/bin/bash

sudo apt-get install -y apache2

# Stop Apache; it starts after it is installed
sudo /etc/init.d/apache2 stop