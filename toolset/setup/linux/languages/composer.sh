#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists bin/composer.phar
[ $? -ne 0 ] || { echo "Composer is installed!"; return 0; }

fw_depends php
fw_get https://getcomposer.org/installer -O composer-installer.php
php composer-installer.php --install-dir=bin
