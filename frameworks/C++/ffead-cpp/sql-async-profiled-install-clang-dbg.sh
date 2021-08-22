cd $IROOT/ffead-cpp-src/

rm -rf build
mkdir build
cd build
CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake -DSRV_EMB=on -DMOD_REDIS=on -DDEBUG=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-6.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake |CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake |g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
#sed -i 's|-fprofile-instr-generate=/tmp/cprof.prof|-fprofile-instr-generate=/tmp/cprofdi.prof|g' $IROOT/ffead-cpp-sql-raw/rtdcf/CMakeLists.txt.template
apt update -yqq && apt install -yqq vim gdb net-tools telnet iputils-ping
./install_ffead-cpp-sql-raw-profiled.sh async

#mv $IROOT/ffead-cpp-sql-raw $IROOT/ffead-cpp-6.0-sql
#sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-6.0-sql/web/te-benchmark-um-pq-async/config/sdorm.xml

