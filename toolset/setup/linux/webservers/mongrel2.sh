. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/bin/mongrel2
[ $? -ne 0 ] || { echo "Mongrel2 is installed!"; return 0; }

fw_depends zeromq

# Dependencies
sudo apt-get install -y sqlite3 libsqlite3-dev uuid uuid-runtime uuid-dev

fw_get https://github.com/zedshaw/mongrel2/tarball/v1.8.1 -O mongrel2.tar.gz
fw_untar mongrel2.tar.gz

# mongrel2 untars into this folder 
mv zedshaw-mongrel2-aa2ecf8 mongrel2

# for zmq4, we update the following file manually (not in v1.8.1)
fw_get https://raw.github.com/zedshaw/mongrel2/9b565eeea003783c47502c2d350b99c9684ce97c/src/zmq_compat.h
mv -f zmq_compat.h mongrel2/src/

cd mongrel2
make clean all
sudo make install

# Update linker cache
sudo ldconfig -v
