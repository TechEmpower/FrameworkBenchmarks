. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/bin/mvn
[ $? -ne 0 ] || { echo "Maven is installed!"; return 0; }

sudo apt-get -y install maven -qq
mvn -version
