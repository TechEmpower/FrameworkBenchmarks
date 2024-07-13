export FFEAD_CPP_PATH=${IROOT}/ffead-cpp-7.0-sql
export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH

if [ "$1" = "async" ]
then
	rm -rf $FFEAD_CPP_PATH/web/t1 $FFEAD_CPP_PATH/web/t2 $FFEAD_CPP_PATH/web/t3 $FFEAD_CPP_PATH/web/t5 $FFEAD_CPP_PATH/web/t6 $FFEAD_CPP_PATH/web/t7
	sed -i 's|<async>false</async>|<async>true</async>|g' $FFEAD_CPP_PATH/web/t4/config/sdorm.xml
	sed -i 's|tfb-database|localhost|g' $FFEAD_CPP_PATH/web/t4/config/sdorm.xml
elif [ "$1" = "async-pool" ]
then
	rm -rf $FFEAD_CPP_PATH/web/t1 $FFEAD_CPP_PATH/web/t2 $FFEAD_CPP_PATH/web/t3 $FFEAD_CPP_PATH/web/t4 $FFEAD_CPP_PATH/web/t6 $FFEAD_CPP_PATH/web/t7
	sed -i 's|<async>false</async>|<async>true</async>|g' $FFEAD_CPP_PATH/web/t5/config/sdorm.xml
	sed -i 's|tfb-database|localhost|g' $FFEAD_CPP_PATH/web/t5/config/sdorm.xml
elif [ "$1" = "wire" ]
then
	rm -rf $FFEAD_CPP_PATH/web/t1 $FFEAD_CPP_PATH/web/t2 $FFEAD_CPP_PATH/web/t3 $FFEAD_CPP_PATH/web/t4 $FFEAD_CPP_PATH/web/t5 $FFEAD_CPP_PATH/web/t7
	sed -i 's|<async>false</async>|<async>true</async>|g' $FFEAD_CPP_PATH/web/t6/config/sdorm.xml
	sed -i 's|tfb-database|localhost|g' $FFEAD_CPP_PATH/web/t6/config/sdorm.xml
	sed -i 's|<wire>false</wire>|<wire>true</wire>|g' $FFEAD_CPP_PATH/web/t6/config/sdorm.xml
elif [ "$1" = "async-wire" ]
then
	rm -rf $FFEAD_CPP_PATH/web/t1 $FFEAD_CPP_PATH/web/t2 $FFEAD_CPP_PATH/web/t3 $FFEAD_CPP_PATH/web/t4 $FFEAD_CPP_PATH/web/t5 $FFEAD_CPP_PATH/web/t6
	sed -i 's|<async>false</async>|<async>true</async>|g' $FFEAD_CPP_PATH/web/t7/config/sdorm.xml
	sed -i 's|tfb-database|localhost|g' $FFEAD_CPP_PATH/web/t7/config/sdorm.xml
	sed -i 's|<wire>false</wire>|<wire>true</wire>|g' $FFEAD_CPP_PATH/web/t7/config/sdorm.xml
else
	rm -rf $FFEAD_CPP_PATH/web/t1 $FFEAD_CPP_PATH/web/t2 $FFEAD_CPP_PATH/web/t4 $FFEAD_CPP_PATH/web/t5 $FFEAD_CPP_PATH/web/t6 $FFEAD_CPP_PATH/web/t7
	sed -i 's|tfb-database|localhost|g' ${FFEAD_CPP_PATH}/web/t3/config/sdorm.xml
	sed -i 's|"TeBkUmLpqRouter"|"TeBkUmLpqRouterPicoV"|g' ${FFEAD_CPP_PATH}/web/t3/config/application.xml
fi


cd $IROOT/lang-server-backends/v/pico.v
v -enable-globals -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -no-pie -flto -fprofile-dir=/tmp/profile-data -fprofile-generate -lgcov --coverage' main.v

#Start postgresql
service postgresql start
#For profiling/benchmarking

cd $IROOT/
./install_ffead-cpp-sql-raw-v-picov-profiled.sh "$1"

cd $IROOT/lang-server-backends/v/pico.v
v -enable-globals -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -no-pie -flto  -fprofile-dir=/tmp/profile-data -fprofile-use=/tmp/profile-data -fprofile-correction -lgcov --coverage' main.v

cd $IROOT/
./install_ffead-cpp-sql-raw-v-picov-profiled.sh "$1"

if [ "$1" = "async" ] || [ "$1" = "async-wire" ]
then
	sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-7.0-sql/web/t4/config/sdorm.xml
	mv $IROOT/lang-server-backends/v/pico.v/main $IROOT/main_async
elif [ "$1" = "async-pool" ]
then
	sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-7.0-sql/web/t4/config/sdorm.xml
	mv $IROOT/lang-server-backends/v/pico.v/main $IROOT/main_async_pool
else
	sed -i 's|localhost|tfb-database|g' $IROOT/ffead-cpp-7.0-sql/web/t3/config/sdorm.xml
	mv $IROOT/lang-server-backends/v/pico.v/main $IROOT/
fi

apt remove -yqq postgresql-${PG_VERSION} postgresql-contrib-${PG_VERSION} gnupg lsb-release && apt autoremove -yqq
rm -rf /ssd/postgresql && rm -rf /tmp/postgresql && rm -rf /tmp/wrk /usr/local/bin/wrk
