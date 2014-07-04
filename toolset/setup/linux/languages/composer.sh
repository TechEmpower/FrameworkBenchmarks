#!/bin/bash

fw_exists bin/composer.phar
[ $? -ne 0 ] || { return 0; }

fw_depends php
fw_get https://getcomposer.org/installer -O composer-installer.php
php composer-installer.php --install-dir=bin
