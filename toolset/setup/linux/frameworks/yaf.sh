#!/bin/bash

fw_depends php5

fw_installed yaf && return 0

# Enable the PHP yaf extension
sed -i 's|;extension=yaf.so|extension=yaf.so|g' $PHP_HOME/lib/php.ini
printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf-2.2.9

echo "" > $IROOT/yaf.installed

source $IROOT/yaf.installed
