#!/bin/bash

fw_depends php7

fw_installed yaf && return 0

# Enable the PHP yaf extension
sed -i 's|;extension=yaf.so|extension=yaf.so|g' $PHP_HOME/lib/php.ini
printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf-3.0.6

echo "" > $IROOT/yaf.installed

source $IROOT/yaf.installed
