#!/bin/bash

RETCODE=$(fw_exists bin/composer.phar)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends php
fw_get https://getcomposer.org/installer -O composer-installer.php
mkdir -p bin
php composer-installer.php --install-dir=bin
