cd ${IROOT}/ffead-cpp-src/
if [ ! -d "ffead-cpp-7.0-bin" ]
then
	exit 1
fi

cd ffead-cpp-7.0-bin
#cache related dockerfiles will add the cache.xml accordingly whenever needed
chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
./server.sh &
COUNTER=0
while [ ! -f lib/libinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure...."
    	exit 1
    fi
done
COUNTER=0
while [ ! -f lib/libdinter.so ]
do
    sleep 1
    COUNTER=$((COUNTER+1))
    if [ "$COUNTER" = 120 ]
    then
    	cat logs/jobs.log
    	echo "ffead-cpp exiting exiting due to failure....ddlib"
    	exit 1
    fi
done
echo "ffead-cpp start successful"
sleep 20
cd tests && rm -f test.csv && cp ${IROOT}/ffead-cpp-src/tests/test-te.csv test.csv && chmod +x *.sh && ./runTests.sh
echo "ffead-cpp normal shutdown"
pkill ffead-cpp

cd ${IROOT}/ffead-cpp-src/
cp -rf ffead-cpp-7.0-bin ${IROOT}/ffead-cpp-7.0-sql
rm -rf ffead-cpp-7.0-bin

cd ${IROOT}/ffead-cpp-7.0-sql

chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh
chmod 755 *.sh
rm -f *.cntrl
rm -f tmp/*.sess