#!/bin/sh

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

if [ $2 = "redis" ]
then
	service redis-server start
	cp -f web/te-benchmark-um/config/cacheredis.xml web/te-benchmark-um/config/cache.xml
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ $2 = "memcached" ]
then
	service memcached start
	cp -f web/te-benchmark-um/config/cachememcached.xml web/te-benchmark-um/config/cache.xml
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ $2 = "mongo" ]
then
	cp -f web/te-benchmark-um/config/sdormmongo.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ $2 = "mysql" ]
then
	cp -f web/te-benchmark-um/config/sdormmysql.xml web/te-benchmark-um/config/sdorm.xml
fi

if [ $2 = "postgresql" ]
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

if [ $1 = "emb" ]
then
	./ffead-cpp $FFEAD_CPP_PATH
fi

if [ $1 = "apache" ]
then
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/cache.xml
	apachectl -D FOREGROUND
fi

if [ $1 = "nginx" ]
then
	sed -i 's|<pool-size>30</pool-size>|<pool-size>3</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/sdorm.xml
	sed -i 's|<pool-size>10</pool-size>|<pool-size>2</pool-size>|g' $FFEAD_CPP_PATH/web/te-benchmark-um/config/cache.xml
	nginx -g 'daemon off;'
fi

wait
