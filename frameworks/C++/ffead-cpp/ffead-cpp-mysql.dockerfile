FROM techempower/ffead-cpp-base:0.1

WORKDIR ${IROOT}/ffead-cpp-src/

RUN cp -f web/te-benchmark/sql-src/TeBkWorldsql.h web/te-benchmark/include/TeBkWorld.h
RUN cp -f web/te-benchmark/sql-src/TeBkWorldsql.cpp web/te-benchmark/src/TeBkWorld.cpp
RUN cp -f web/te-benchmark/config/sdormmysql.xml web/te-benchmark/config/sdorm.xml
RUN rm -rf ffead-cpp-2.0-bin
RUN make build-apps
RUN rm -rf ${IROOT}/ffead-cpp-2.0
RUN cp -rf ffead-cpp-2.0-bin ${IROOT}/ffead-cpp-2.0

WORKDIR ${IROOT}/ffead-cpp-2.0

RUN rm -rf web/default web/oauthApp web/flexApp web/markers
RUN chmod 755 *.sh resources/*.sh rtdcf/autotools/*.sh

RUN chmod 755 $FFEAD_CPP_PATH/*.sh
RUN rm -f $FFEAD_CPP_PATH/*.cntrl
RUN rm -f $FFEAD_CPP_PATH/tmp/*.sess

CMD ./server.sh
