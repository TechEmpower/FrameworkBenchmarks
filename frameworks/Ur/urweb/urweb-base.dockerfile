FROM tfb/base:latest

COPY ./ ./

ENV URWEB_VERSION=20160621
ENV COMPILER=/urweb-build

RUN apt install -yqq mlton libssl-dev libpq-dev
RUN mkdir -p $COMPILER && \
    wget http://www.impredicative.com/ur/urweb-$URWEB_VERSION.tgz && \
    tar xvf urweb-$URWEB_VERSION.tgz && \
    cd urweb-$URWEB_VERSION && \
    ./configure --prefix=$COMPILER && \
    make && \
    make install

ENV URWEB_HOME=${COMPILER}
ENV LD_LIBRARY_PATH=${COMPILER}/lib
ENV PATH=${COMPILER}/bin:${PATH}

RUN urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=TFB-database" bench
