export FFEAD_CPP_PATH=${IROOT}/ffead-cpp-6.0-sql
export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH

sed -i 's|tfb-database|localhost|g' $IROOT/ffead-cpp-6.0-sql/web/te-benchmark-um-pq/config/sdorm.xml

cd $IROOT/lang-server-backends/v/pico.v
v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -no-pie -flto -fprofile-dir=/tmp/profile-data -fprofile-generate -lgcov --coverage' main.v

#Start postgresql
service postgresql start
#For profiling/benchmarking

cd $IROOT/
./install_ffead-cpp-sql-raw-v-picov-profiled.sh

cd $IROOT/lang-server-backends/v/pico.v
v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -no-pie -flto  -fprofile-dir=/tmp/profile-data -fprofile-use=/tmp/profile-data -fprofile-correction -lgcov --coverage' main.v

cd $IROOT/
./install_ffead-cpp-sql-raw-v-picov-profiled.sh

mv $IROOT/lang-server-backends/v/pico.v/main $IROOT/

sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-6.0-sql/web/te-benchmark-um-pq/config/sdorm.xml

apt remove -yqq postgresql-13 postgresql-contrib-13 gnupg lsb-release
apt autoremove -yqq
rm -rf /ssd/postgresql
rm -rf /tmp/postgresql
rm -rf /tmp/wrk /usr/local/bin/wrk
