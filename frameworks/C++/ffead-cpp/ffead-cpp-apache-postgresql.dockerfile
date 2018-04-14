FROM buildpack-deps:xenial

ENV IROOT=/installs
ENV FFEAD_CPP_PATH=${IROOT}/ffead-cpp-2.0
ENV PATH=${FFEAD_CPP_PATH}:${PATH}

RUN mkdir /installs

RUN apt update -yqq && apt install -yqq unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev

WORKDIR $IROOT

WORKDIR /

COPY te-benchmark/ te-benchmark/
COPY ffead-cpp-framework.sh ./
COPY server.sh ./

RUN chmod 755 *.sh

RUN sed -i 's|--enable-mod_sdormmongo=yes||g' ffead-cpp-framework.sh

RUN ./ffead-cpp-framework.sh

COPY ffead-cpp-httpd.sh ./

RUN chmod 755 *.sh

RUN ./ffead-cpp-httpd.sh

ENV PATH=${IROOT}/httpd/bin:${PATH}

WORKDIR ${IROOT}/ffead-cpp-src/

RUN cp -f web/te-benchmark/sql-src/TeBkWorldsql.h web/te-benchmark/include/TeBkWorld.h
RUN cp -f web/te-benchmark/sql-src/TeBkWorldsql.cpp web/te-benchmark/src/TeBkWorld.cpp
RUN cp -f web/te-benchmark/config/sdormpostgresql.xml web/te-benchmark/config/sdorm.xml
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

CMD apachectl -D FOREGROUND
