. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/nginx/sbin/nginx
[ $? -ne 0 ] || { echo "Nginx is installed!"; return 0; }

fw_get http://nginx.org/download/nginx-1.4.1.tar.gz
fw_untar nginx-1.4.1.tar.gz
cd nginx-1.4.1
./configure
make
sudo make install
