#!/bin/bash

fw_depends mysql
sudo add-apt-repository -y ppa:ondrej/php
sudo apt-get update
sudo apt-get -y --force-yes install php-cli php-mysql
sudo add-apt-repository -y --remove ppa:ondrej/php
php -r 'file_put_contents("composer.phar", file_get_contents("https://getcomposer.org/composer.phar"));'
php composer.phar update
php $TROOT/server.php start -d
