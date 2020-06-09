#!/bin/sh

rm -f /usr/local/lib/libffead-*
rm -f /usr/local/lib/libte_benc*
rm -f /usr/local/lib/libinter.so
rm -f /usr/local/lib/libdinter.so

export FFEAD_CPP_PATH=${IROOT}/$1

#Set the libraries based on the ENV variable FFEAD_CPP_PATH
ln -s ${FFEAD_CPP_PATH}/lib/libte_benchmark_um.so /usr/local/lib/libte_benchmark_um.so
ln -s ${FFEAD_CPP_PATH}/lib/libffead-modules.so /usr/local/lib/libffead-modules.so
ln -s ${FFEAD_CPP_PATH}/lib/libffead-framework.so /usr/local/lib/libffead-framework.so
ln -s ${FFEAD_CPP_PATH}/lib/libinter.so /usr/local/lib/libinter.so
ln -s ${FFEAD_CPP_PATH}/lib/libdinter.so /usr/local/lib/libdinter.so
ldconfig

echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo 'echo never > /sys/kernel/mm/transparent_hugepage/enabled' >> /etc/rc.local
sysctl vm.overcommit_memory=1

export PATH=${IROOT}/nginxfc/sbin:${PATH}
export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH
export ODBCINI=${IROOT}/odbc.ini
export ODBCSYSINI=${IROOT}

cd $FFEAD_CPP_PATH

#use below settings only for debugging
#echo '/tmp/core.%h.%e.%t' > /proc/sys/kernel/core_pattern
#ulimit -c unlimited

service redis-server stop
service apache2 stop
service memcached stop

rm -f /tmp/cache.lock
rm -f web/te-benchmark-um/config/cache.xml

if [ "$3" = "redis" ]
then
	service redis-server start
	cp -f web/te-benchmark-um/config/cacheredis.xml web/te-benchmark-um/config/cache.xml
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ "$3" = "memcached" ]
then
	service memcached start
	cp -f web/te-benchmark-um/config/cachememcached.xml web/te-benchmark-um/config/cache.xml
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ "$3" = "mongo" ]
then
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ "$3" = "mysql" ]
then
	cp -f web/te-benchmark-um/config/sdormmysql.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ "$3" = "postgresql" ]
then
	cp -f web/te-benchmark-um/config/sdormpostgresql.xml web/te-benchmark-um/config/sdorm.xml
fi

rm -f rtdcf/*.d rtdcf/*.o 
rm -f *.cntrl
rm -f tmp/*.sess
if [ ! -d tmp ]; then
mkdir tmp
fi
chmod 700 ffead-cpp*
chmod 700 resources/*.sh
chmod 700 tests/*
chmod 700 rtdcf/*

if [ "$2" = "emb" ]
then
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' $FFEAD_CPP_PATH/resources/server.prop
	for i in $(seq 0 $(($(nproc --all)-1))); do
	  taskset -c $i ./ffead-cpp $FFEAD_CPP_PATH &
	done
fi

if [ "$2" = "lithium" ]
then
	./ffead-cpp-lithium $FFEAD_CPP_PATH &
fi

if [ "$2" = "cinatra" ]
then
	./ffead-cpp-cinatra $FFEAD_CPP_PATH &
fi

if [ "$2" = "drogon" ]
then
	./ffead-cpp-drogon $FFEAD_CPP_PATH &
fi

if [ "$2" = "apache" ]
then
	if [ "$3" = "mysql" ] || [ "$3" = "postgresql" ]
	then
		sed -i 's|/installs/ffead-cpp-4.0|'/installs/ffead-cpp-4.0-sql'|g' /etc/apache2/apache2.conf
		sed -i 's|/installs/ffead-cpp-4.0|'/installs/ffead-cpp-4.0-sql'|g' /etc/apache2/sites-enabled/000-default.conf /etc/apache2/sites-enabled/ffead-site.conf
	fi
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/cache.xml
	apachectl -D FOREGROUND
fi

if [ "$2" = "nginx" ]
then
	if [ "$3" = "mysql" ] || [ "$3" = "postgresql" ]
	then
		sed -i 's|/installs/ffead-cpp-4.0/|'/installs/ffead-cpp-4.0-sql/'|g' ${IROOT}/nginxfc/conf/nginx.conf
	fi
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/cache.xml
	nginx -g 'daemon off;'
fi

if [ "$2" = "libreactor" ]
then
	cd ${IROOT}
	./libreactor-ffead-cpp $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "crystal-http" ]
then
	cd ${IROOT}
	./crystal-ffead-cpp.out --ffead-cpp-dir=$FFEAD_CPP_PATH --to=8080
fi

if [ "$2" = "crystal-h2o" ]
then
	cd ${IROOT}
	./h2o-evloop-ffead-cpp.out --ffead-cpp-dir=$FFEAD_CPP_PATH --to=8080
fi

if [ "$2" = "rust-actix" ]
then
	cd ${IROOT}
	./actix-ffead-cpp $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "rust-hyper" ]
then
	cd ${IROOT}
	./hyper-ffead-cpp $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "rust-thruster" ]
then
	cd ${IROOT}
	./thruster-ffead-cpp $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "rust-rocket" ]
then
	cd ${IROOT}
	./rocket-ffead-cpp $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "go-fasthttp" ]
then
	cd ${IROOT}
	./fasthttp-ffead-cpp --server_directory=$FFEAD_CPP_PATH -addr=8080
fi

if [ "$2" = "go-gnet" ]
then
	cd ${IROOT}
	./gnet-ffead-cpp --server_directory=$FFEAD_CPP_PATH --port=8080
fi

if [ "$2" = "v-vweb" ]
then
	cd ${IROOT}
	./vweb --server_dir=$FFEAD_CPP_PATH --server_port=8080
fi

if [ "$2" = "v-picov" ]
then
	cd ${IROOT}
	./main --server_dir=$FFEAD_CPP_PATH --server_port=8080
fi

if [ "$2" = "java-firenio" ]
then
	cd ${IROOT}
	java -classpath firenio-ffead-cpp-0.1-jar-with-dependencies.jar com.firenio.ffeadcpp.FirenioFfeadCppServer $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "java-rapidoid" ]
then
	cd ${IROOT}
	java -classpath rapidoid-ffead-cpp-1.0-jar-with-dependencies.jar com.rapidoid.ffeadcpp.Main $FFEAD_CPP_PATH 8080
fi

if [ "$2" = "java-wizzardo-http" ]
then
	cd ${IROOT}
	java -jar wizzardo-ffead-cpp-all-1.0.jar $FFEAD_CPP_PATH 8080
fi

wait
