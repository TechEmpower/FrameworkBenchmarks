cd $IROOT/ffead-cpp-src/

rm -rf build
mkdir build
cd build
CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS="-march=native -flto -fprofile-instr-generate=/tmp/cprof.prof" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-6.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake |CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake |g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
#sed -i 's|-fprofile-instr-generate=/tmp/cprof.prof|-fprofile-instr-generate=/tmp/cprofdi.prof|g' $IROOT/ffead-cpp-sql-raw/rtdcf/CMakeLists.txt.template
./install_ffead-cpp-sql-raw-profiled.sh
rm -rf $IROOT/ffead-cpp-sql-raw

cd $IROOT/ffead-cpp-src
rm -rf build
mkdir build
cd build
llvm-profdata-10 merge -output=/tmp/cprof.pgo  /tmp/cprof.prof
#llvm-profdata-10 merge -output=/tmp/cprofdi.pgo  /tmp/cprofdi.prof
ls -ltr /tmp/cprof*
CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS="-march=native -flto -fprofile-instr-use=/tmp/cprof.pgo" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-6.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake |CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake |g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
#sed -i 's|-fprofile-instr-use=/tmp/cprof.pgo|-fprofile-instr-use=/tmp/cprofdi.pgo|g' $IROOT/ffead-cpp-sql-raw/rtdcf/CMakeLists.txt.template
./install_ffead-cpp-sql-raw-profiled.sh
mv $IROOT/ffead-cpp-sql-raw $IROOT/ffead-cpp-6.0-sql

sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-6.0-sql/web/te-benchmark-um-pq/config/sdorm.xml

apt remove -yqq postgresql-13 postgresql-contrib-13 gnupg lsb-release
apt autoremove -yqq
rm -rf /ssd/postgresql
rm -rf /tmp/postgresql
rm -rf /tmp/wrk /usr/local/bin/wrk

