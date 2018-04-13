FROM buildpack-deps:xenial

ENV IROOT=/installs
ENV FFEAD_CPP_PATH=${IROOT}/ffead-cpp-2.0
ENV PATH=${FFEAD_CPP_PATH}:${PATH}

RUN mkdir /installs

RUN apt update -yqq && apt install -yqq unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev

WORKDIR /

COPY te-benchmark/ te-benchmark/
COPY ffead-cpp-framework.sh ./
COPY server.sh ./

RUN chmod 755 *.sh

RUN sed -i 's|--enable-mod_sdormmongo=yes||g' ffead-cpp-framework.sh

RUN ./ffead-cpp-framework.sh

WORKDIR $IROOT

RUN wget -q http://nginx.org/download/nginx-1.13.1.tar.gz
RUN tar xf nginx-1.13.1.tar.gz

WORKDIR $IROOT/nginx-1.13.1

RUN ./configure \
    --prefix=${IROOT}/nginxfc \
    --with-ld-opt="-lstdc++ -L${IROOT}/ffead-cpp-2.0/lib -L${IROOT}" \
    --add-module="${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp" \
    --with-cc-opt="-I${IROOT}/ffead-cpp-2.0/include -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -w -fpermissive"
RUN make
RUN make install

RUN cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginxfc/conf/
RUN sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf

ENV PATH=${IROOT}/nginxfc/sbin:${PATH}

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

CMD nginx -g 'daemon off;'
