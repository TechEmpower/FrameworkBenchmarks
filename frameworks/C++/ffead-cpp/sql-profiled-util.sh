mkdir /tmp/profile-data

rm -rf $IROOT/ffead-cpp-6.0-sql

if [ "$1" = "batch" ]
then
	apt remove -yqq libpq-dev
	apt autoremove -yqq
	rm -f /usr/lib/x86_64-linux-gnu/libpq.*
	apt update && apt install -y bison flex libreadline-dev
	cd /tmp
	#wget -q https://github.com/an-tao/postgres/archive/batch_mode_ubuntu.tar.gz
	#tar -xzf batch_mode_ubuntu.tar.gz
	#cd postgres-batch_mode_ubuntu
	#./configure --prefix=/usr CFLAGS='-O2 -pipe -march=native'
	#make && make install
	wget -nv https://github.com/postgres/postgres/archive/b787d4ce6d910080065025bcd5f968544997271f.zip
	unzip -q b787d4ce6d910080065025bcd5f968544997271f.zip
	cd postgres-b787d4ce6d910080065025bcd5f968544997271f
	wget -nv https://www.postgresql.org/message-id/attachment/115223/v22-0001-libpq-batch.patch
	git apply ./v22-0001-libpq-batch.patch
	./configure --prefix=/usr CFLAGS='-O3 -march=native -flto'
	cd src/interfaces/libpq
	make all install -j4
	cp ../../../src/include/postgres_ext.h ../../../src/include/pg_config_ext.h libpq-fe.h /usr/include
fi

if [ "$2" = "clang" ]
then
	apt update -yqq && apt install -yqq clang
fi

cd $IROOT/ffead-cpp-src/
rm -rf $IROOT/ffead-cpp-sql-raw
rm -rf CMakeCache.txt CMakeFiles
rm -rf web/te-benchmark-um web/te-benchmark-um-mgr

sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um)||g' CMakeLists.txt
sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-mgr)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um/libte-benchmark-um${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-mgr/libte-benchmark-um-mgr${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt

if [ "$3" = "async" ]
then
	sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-pq)||g' CMakeLists.txt
	sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-pq/libte-benchmark-um-pq${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
	sed -i 's|tfb-database|localhost|g' $IROOT/ffead-cpp-src/web/te-benchmark-um-pq-async/config/sdorm.xml
	rm -rf web/te-benchmark-um-pq
else
	sed -i 's|add_subdirectory(${PROJECT_SOURCE_DIR}/web/te-benchmark-um-pq-async)||g' CMakeLists.txt
	sed -i 's|install(FILES ${PROJECT_BINARY_DIR}/web/te-benchmark-um-pq-async/libte-benchmark-um-pq-async${LIB_EXT} DESTINATION ${PROJECT_NAME}-bin/lib)||g' CMakeLists.txt
	sed -i 's|tfb-database|localhost|g' $IROOT/ffead-cpp-src/web/te-benchmark-um-pq/config/sdorm.xml
	rm -rf web/te-benchmark-um-pq-async
fi
