mkdir /tmp/profile-data

rm -rf $IROOT/ffead-cpp-5.0-sql

if [ "$1" = "batch" ]
then
	apt remove -yqq libpq-dev
	apt autoremove -yqq
	apt update && apt install -y bison flex libreadline-dev
	cd /tmp
	wget -q https://github.com/an-tao/postgres/archive/batch_mode_ubuntu.tar.gz
	tar -xzf batch_mode_ubuntu.tar.gz
	cd postgres-batch_mode_ubuntu
	./configure --prefix=/usr CFLAGS='-O2 -pipe -march=native'
	make && make install
fi

apt update -yqq && apt install -yqq clang

cd $IROOT/ffead-cpp-src/
rm -rf CMakeCache.txt CMakeFiles
rm -rf web/te-benchmark-um web/te-benchmark-um-mgr web/te-benchmark-um-pq

sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-mgr)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-pq)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um/libte_benchmark_um${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-mgr/libte_benchmark_um_mgr${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-pq/libte_benchmark_um_pq${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt

sed -i 's|tfb-database|localhost|g' $IROOT/ffead-cpp-src/web/te-benchmark-um-pq-async/config/sdorm.xml

rm -rf build
mkdir build
cd build
CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS="-march=native -flto -fprofile-instr-generate=/tmp/cprof.prof" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-5.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake .|CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake .|g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
#sed -i 's|-fprofile-instr-generate=/tmp/cprof.prof|-fprofile-instr-generate=/tmp/cprofdi.prof|g' $IROOT/ffead-cpp-sql-raw/rtdcf/CMakeLists.txt.template
./install_ffead-cpp-sql-raw-profiled.sh async
rm -rf $IROOT/ffead-cpp-sql-raw

cd $IROOT/ffead-cpp-src
rm -rf build
mkdir build
cd build
llvm-profdata-10 merge -output=/tmp/cprof.pgo  /tmp/cprof.prof
#llvm-profdata-10 merge -output=/tmp/cprofdi.pgo  /tmp/cprofdi.prof
ls -ltr /tmp/cprof*
CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS="-march=native -flto -fprofile-instr-use=/tmp/cprof.pgo" cmake -DSRV_EMB=on -DMOD_MEMCACHED=on -DMOD_REDIS=on -DMOD_SDORM_MONGO=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-5.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake .|CC=/usr/bin/clang CXX=/usr/bin/clang++ cmake .|g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
#sed -i 's|-fprofile-instr-use=/tmp/cprof.pgo|-fprofile-instr-use=/tmp/cprofdi.pgo|g' $IROOT/ffead-cpp-sql-raw/rtdcf/CMakeLists.txt.template
./install_ffead-cpp-sql-raw-profiled.sh async
mv $IROOT/ffead-cpp-sql-raw $IROOT/ffead-cpp-5.0-sql

sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-5.0-sql/web/te-benchmark-um-pq-async/config/sdorm.xml

apt remove -yqq postgresql-13 postgresql-contrib-13 gnupg lsb-release
apt autoremove -yqq
rm -rf /ssd/postgresql
rm -rf /tmp/postgresql
rm -rf /tmp/wrk /usr/local/bin/wrk

