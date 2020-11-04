mkdir /tmp/profile-data

rm -rf $IROOT/ffead-cpp-5.0-sql

if [ "$1" = "batch" ]
then
	apt remove -y libpq-dev
	apt autoremove -y
	apt update && apt install -y bison flex
	cd /tmp
	commit=b787d4ce6d910080065025bcd5f968544997271f
	wget -nv https://github.com/postgres/postgres/archive/$commit.zip
	unzip -q $commit.zip
	cd postgres-$commit
	wget -nv https://www.postgresql.org/message-id/attachment/115223/v22-0001-libpq-batch.patch
	git apply ./v22-0001-libpq-batch.patch
	./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
	cd src/interfaces/libpq
	make all install -j4
	cp ../../../src/include/postgres_ext.h ../../../src/include/pg_config_ext.h /usr/include
fi 

cd $IROOT/ffead-cpp-src/
rm -rf CMakeCache.txt CMakeFiles
rm -rf web/te-benchmark-um web/te-benchmark-um-mgr web/te-benchmark-um-pq-async

sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-mgr)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-pq-async)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um/libte_benchmark_um${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-mgr/libte_benchmark_um_mgr${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-pq-async/libte_benchmark_um_pq_async${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt

sed -i 's|tfb-database|localhost|g' $IROOT/ffead-cpp-src/web/te-benchmark-um-pq/config/sdorm.xml

rm -rf build
mkdir build
cd build
CXXFLAGS="-march=native -fprofile-dir=/tmp/profile-data -fprofile-generate" cmake -DSRV_EMB=on -DMOD_REDIS=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-5.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake .|cmake -DCMAKE_EXE_LINKER_FLAGS="-fprofile-dir=/tmp/profile-data -fprofile-generate" -DCMAKE_CXX_FLAGS="-march=native -fprofile-dir=/tmp/profile-data  -fprofile-generate" .|g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
./install_ffead-cpp-sql-raw-profiled.sh
rm -rf $IROOT/ffead-cpp-sql-raw

cd $IROOT/ffead-cpp-src
rm -rf build
mkdir build
cd build
CXXFLAGS="-march=native -fprofile-dir=/tmp/profile-data -fprofile-use=/tmp/profile-data -fprofile-correction" cmake -DSRV_EMB=on -DMOD_MEMCACHED=on -DMOD_REDIS=on -DMOD_SDORM_MONGO=on ..
make install && mv $IROOT/ffead-cpp-src/ffead-cpp-5.0-bin $IROOT/ffead-cpp-sql-raw

#Start postgresql
service postgresql stop
#For profiling/benchmarking

cd $IROOT/
sed -i 's|cmake .|CXXFLAGS="-march=native -fprofile-dir=/tmp/profile-data -fprofile-use -fprofile-correction" cmake .|g' $IROOT/ffead-cpp-sql-raw/resources/rundyn-automake.sh
./install_ffead-cpp-sql-raw-profiled.sh
mv $IROOT/ffead-cpp-sql-raw $IROOT/ffead-cpp-5.0-sql

sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-5.0-sql/web/te-benchmark-um-pq/config/sdorm.xml

apt remove -y postgresql-13 postgresql-contrib-13 locales gnupg lsb-release
apt autoremove -y
rm -rf /ssd/postgresql
rm -rf /tmp/postgresql
rm -rf /tmp/wrk /usr/local/bin/wrk

