#!/bin/sh

rm -f /usr/local/lib/libffead-*
rm -f /usr/local/lib/libte_benc*
rm -f /usr/local/lib/libinter.so
rm -f /usr/local/lib/libdinter.so

export FFEAD_CPP_PATH=${IROOT}/$1

ln -s ${FFEAD_CPP_PATH}/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so
ln -s ${FFEAD_CPP_PATH}/lib/libte-benchmark-um-pq.so /usr/local/lib/libte-benchmark-um-pq.so
ln -s ${FFEAD_CPP_PATH}/lib/libte-benchmark-um-mgr.so /usr/local/lib/libte-benchmark-um-mgr.so
ln -s ${FFEAD_CPP_PATH}/lib/libte-benchmark-um-pq-async.so /usr/local/lib/libte-benchmark-um-pq-async.so
ln -s ${FFEAD_CPP_PATH}/lib/libffead-modules.so /usr/local/lib/libffead-modules.so
ln -s ${FFEAD_CPP_PATH}/lib/libffead-framework.so /usr/local/lib/libffead-framework.so
ln -s ${FFEAD_CPP_PATH}/lib/libinter.so /usr/local/lib/libinter.so
ln -s ${FFEAD_CPP_PATH}/lib/libdinter.so /usr/local/lib/libdinter.so
ldconfig

if [ "$2" = "nginx" ]
then
	if [ "$3" = "mysql" ] || [ "$3" = "postgresql" ]
	then
		export PATH=${IROOT}/nginx-ffead-sql/sbin:${PATH}
	else
		export PATH=${IROOT}/nginx-ffead-mongo/sbin:${PATH}
	fi
fi

export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH
export ODBCINI=${IROOT}/odbc.ini
export ODBCSYSINI=${IROOT}
export LD_PRELOAD=/usr/local/lib/libmimalloc.so
#export LD_PRELOAD=$IROOT/snmalloc-0.4.2/build/libsnmallocshim.so

cd $FFEAD_CPP_PATH

#use below settings only for debugging
#echo '/tmp/core.%h.%e.%t' > /proc/sys/kernel/core_pattern
#ulimit -c unlimited

service redis-server stop
service apache2 stop
service memcached stop

if [ "$3" = "mongo" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um
	rm -rf web/te-benchmark-um-mgr web/te-benchmark-um-pq web/te-benchmark-um-pq-async
	cp -f ${WEB_DIR}/config/sdormmongo.xml ${WEB_DIR}/config/sdorm.xml
elif [ "$3" = "mongo-raw" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um-mgr
	rm -rf web/te-benchmark-um web/te-benchmark-um-pq web/te-benchmark-um-pq-async
elif [ "$3" = "mysql" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um
	rm -rf web/te-benchmark-um-mgr web/te-benchmark-um-pq web/te-benchmark-um-pq-async
	cp -f ${WEB_DIR}/config/sdormmysql.xml ${WEB_DIR}/config/sdorm.xml
elif [ "$3" = "postgresql" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um
	rm -rf web/te-benchmark-um-mgr web/te-benchmark-um-pq web/te-benchmark-um-pq-async
	cp -f web/te-benchmark-um/config/sdormpostgresql.xml web/te-benchmark-um/config/sdorm.xml
elif [ "$3" = "postgresql-raw" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um-pq
	rm -rf web/te-benchmark-um web/te-benchmark-um-mgr web/te-benchmark-um-pq-async
	sed -i 's|<async>true</async>|<async>false</async>|g' ${WEB_DIR}/config/sdorm.xml
elif [ "$3" = "postgresql-raw-async" ]
then
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um-pq-async
	rm -rf web/te-benchmark-um web/te-benchmark-um-mgr web/te-benchmark-um-pq
	sed -i 's|<async>false</async>|<async>true</async>|g' ${WEB_DIR}/config/sdorm.xml
else
	WEB_DIR=$FFEAD_CPP_PATH/web/te-benchmark-um
	rm -rf web/te-benchmark-um-mgr web/te-benchmark-um-pq web/te-benchmark-um-pq-async
fi

if [ "$4" = "memory" ]
then
	cp -f ${WEB_DIR}/config/cachememory.xml ${WEB_DIR}/config/cache.xml
elif [ "$4" = "redis" ]
then
	service redis-server start
	cp -f ${WEB_DIR}/config/cacheredis.xml ${WEB_DIR}/config/cache.xml
elif [ "$4" = "memcached" ]
then
	service memcached start
	cp -f ${WEB_DIR}/config/cachememcached.xml ${WEB_DIR}/config/cache.xml
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
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' resources/server.prop
	for i in $(seq 0 $(($(nproc --all)-1))); do
		taskset -c $i ./ffead-cpp $FFEAD_CPP_PATH &
	done
elif [ "$2" = "lithium" ]
then
	./ffead-cpp-lithium $FFEAD_CPP_PATH &
elif [ "$2" = "cinatra" ]
then
	./ffead-cpp-cinatra $FFEAD_CPP_PATH &
elif [ "$2" = "drogon" ]
then
	./ffead-cpp-drogon $FFEAD_CPP_PATH &
elif [ "$2" = "apache" ]
then
	if [ "$3" = "mysql" ] || [ "$3" = "postgresql" ]
	then
		sed -i 's|/installs/ffead-cpp-6.0|'/installs/ffead-cpp-6.0-sql'|g' /etc/apache2/apache2.conf
		sed -i 's|/installs/ffead-cpp-6.0|'/installs/ffead-cpp-6.0-sql'|g' /etc/apache2/sites-enabled/000-default.conf /etc/apache2/sites-enabled/ffead-site.conf
	fi
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' web/te-benchmark-um/config/cache.xml
	apachectl -D FOREGROUND
elif [ "$2" = "nginx" ]
then
	mkdir -p ${IROOT}/nginxfc/logs
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' web/te-benchmark-um/config/cache.xml
	if [ "$3" = "mysql" ] || [ "$3" = "postgresql" ]
	then
		nginx -g 'daemon off;' -c ${IROOT}/nginx-ffead-sql/conf/nginx.conf
	else
		nginx -g 'daemon off;' -c ${IROOT}/nginx-ffead-mongo/conf/nginx.conf
	fi
elif [ "$2" = "libreactor" ]
then
	cd ${IROOT}
	./libreactor-ffead-cpp $FFEAD_CPP_PATH 8080
elif [ "$2" = "h2o" ]
then
	cd ${IROOT}/lang-server-backends/c/h2o
	./h2o.sh ${FFEAD_CPP_PATH} ${LD_LIBRARY_PATH} 8080
elif [ "$2" = "crystal-http" ]
then
	cd ${IROOT}
	for i in $(seq 0 $(($(nproc --all)-1))); do
		taskset -c $i ./crystal-ffead-cpp.out --ffead-cpp-dir=$FFEAD_CPP_PATH --to=8080 &
	done
elif [ "$2" = "crystal-h2o" ]
then
	cd ${IROOT}
	for i in $(seq 0 $(($(nproc --all)-1))); do
	  taskset -c $i ./h2o-evloop-ffead-cpp.out --ffead-cpp-dir=$FFEAD_CPP_PATH --to=8080 &
	done
elif [ "$2" = "julia-http" ]
then
	for i in $(seq 0 $(($(nproc --all)-1))); do
		julia ${IROOT}/lang-server-backends/julia/http.jl/server.jl $FFEAD_CPP_PATH
	done
elif [ "$2" = "swift-nio" ]
then
	cd ${IROOT}
	./app $FFEAD_CPP_PATH
elif [ "$2" = "d-hunt" ]
then
	cd ${IROOT}
	./hunt-minihttp -s $FFEAD_CPP_PATH
elif [ "$2" = "rust-actix" ]
then
	cd ${IROOT}
	./actix-ffead-cpp $FFEAD_CPP_PATH 8080
elif [ "$2" = "rust-hyper" ]
then
	cd ${IROOT}
	./hyper-ffead-cpp $FFEAD_CPP_PATH 8080
elif [ "$2" = "rust-thruster" ]
then
	cd ${IROOT}
	./thruster-ffead-cpp $FFEAD_CPP_PATH 8080
elif [ "$2" = "rust-rocket" ]
then
	cd ${IROOT}
	./rocket-ffead-cpp $FFEAD_CPP_PATH 8080
elif [ "$2" = "go-fasthttp" ]
then
	cd ${IROOT}
	./fasthttp-ffead-cpp --server_directory=$FFEAD_CPP_PATH -addr=8080
elif [ "$2" = "go-gnet" ]
then
	cd ${IROOT}
	./gnet-ffead-cpp --server_directory=$FFEAD_CPP_PATH --port=8080
elif [ "$2" = "v-vweb" ]
then
	cd ${IROOT}
	for i in $(seq 0 $(($(nproc --all)-1))); do
		taskset -c $i ./vweb --server_dir=$FFEAD_CPP_PATH --server_port=8080 &
	done
elif [ "$2" = "v-picov" ]
then
	cd ${IROOT}
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' $FFEAD_CPP_PATH/resources/server.prop
	for i in $(seq 0 $(($(nproc --all)-1))); do
		taskset -c $i ./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 &
	done
elif [ "$2" = "java-firenio" ]
then
	cd ${IROOT}
	java                       \
	    -server                    \
	    -XX:+UseNUMA               \
	    -XX:+UseParallelGC         \
	    -XX:+AggressiveOpts        \
	    -Dlite=false               \
	    -Dcore=1                   \
	    -Dframe=16                 \
	    -DreadBuf=512              \
	    -Dpool=true                \
	    -Ddirect=true              \
	    -Dinline=true              \
	    -Dlevel=1                  \
	    -Dread=false               \
	    -Depoll=true               \
	    -Dnodelay=true             \
	    -Dcachedurl=false          \
	    -DunsafeBuf=true           \
	    -classpath firenio-ffead-cpp-0.1-jar-with-dependencies.jar com.firenio.ffeadcpp.FirenioFfeadCppServer $FFEAD_CPP_PATH 8080
elif [ "$2" = "java-rapidoid" ]
then
	cd ${IROOT}
	java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts \
		-classpath rapidoid-ffead-cpp-1.0-jar-with-dependencies.jar \
		com.rapidoid.ffeadcpp.Main $FFEAD_CPP_PATH 8080 profiles=production
elif [ "$2" = "java-wizzardo-http" ]
then
	cd ${IROOT}
	java -Xmx2G -Xms2G -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts \
		-jar wizzardo-ffead-cpp-all-1.0.jar $FFEAD_CPP_PATH 8080 env=prod
elif [ "$2" = "seastar" ]
then
	cd ${IROOT}/lang-server-backends/c++/seastar
	./ffead-cpp-seastar --port=8080 --address=0.0.0.0 --fcpdir=${FFEAD_CPP_PATH} -c$(nproc)
fi

wait
