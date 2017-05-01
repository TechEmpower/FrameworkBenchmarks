#!/bin/bash

fw_depends php7

fw_installed phalcon && return 0

# Enable the PHP phalcon extension
sed -i 's|;extension=phalcon.so|extension=phalcon.so|g' $PHP_HOME/lib/php.ini

VERSION="3.1.2"

fw_get -O https://github.com/phalcon/cphalcon/archive/v${VERSION}.tar.gz
fw_untar v${VERSION}.tar.gz
cd cphalcon-${VERSION}/build
./install

echo "" > $IROOT/phalcon.installed

source $IROOT/phalcon.installed
