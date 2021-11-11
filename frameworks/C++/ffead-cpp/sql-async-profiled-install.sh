cd $IROOT/ffead-cpp-src/

rm -rf build
mkdir build
cd build
CXXFLAGS="-march=native -flto -fprofile-dir=/tmp/profile-data -fprofile-generate" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-6.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
#sed -i 's|cmake |cmake -DCMAKE_EXE_LINKER_FLAGS="-fprofile-dir=/tmp/profile-data -fprofile-generate" -DCMAKE_CXX_FLAGS="-march=native -fprofile-dir=/tmp/profile-data -fprofile-generate" |g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
./install_ffead-cpp-sql-raw-profiled.sh async
rm -rf $IROOT/ffead-cpp-sql-raw

cd $IROOT/ffead-cpp-src
rm -rf build
mkdir build
cd build
CXXFLAGS="-march=native -flto -fprofile-dir=/tmp/profile-data -fprofile-use=/tmp/profile-data -fprofile-correction" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-6.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
#sed -i 's|cmake |CXXFLAGS="-march=native -fprofile-dir=/tmp/profile-data -fprofile-use -fprofile-correction" cmake |g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
./install_ffead-cpp-sql-raw-profiled.sh async
mv $IROOT/ffead-cpp-sql-raw $IROOT/ffead-cpp-6.0-sql

sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-6.0-sql/web/te-benchmark-um-pq-async/config/sdorm.xml

apt remove -yqq postgresql-13 postgresql-contrib-13 gnupg lsb-release
apt autoremove -yqq
rm -rf /ssd/postgresql
rm -rf /tmp/postgresql
rm -rf /tmp/wrk /usr/local/bin/wrk

