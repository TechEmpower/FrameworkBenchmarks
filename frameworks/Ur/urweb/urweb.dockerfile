FROM ubuntu:16.04

ADD ./ /urweb
WORKDIR /urweb

ENV URWEB_VERSION=20160621
ENV COMPILER=/urweb/urweb-build

RUN apt update -yqq && apt install -yqq build-essential wget mlton libssl-dev libpq-dev libmysqlclient-dev

RUN mkdir -p $COMPILER && \
    wget -q http://www.impredicative.com/ur/urweb-$URWEB_VERSION.tgz && \
    tar xf urweb-$URWEB_VERSION.tgz && \
    cd urweb-$URWEB_VERSION && \
    ./configure --prefix=$COMPILER && \
    make && \
    make install

ENV URWEB_HOME=${COMPILER}
ENV LD_LIBRARY_PATH=${COMPILER}/lib
ENV PATH=${COMPILER}/bin:${PATH}

RUN urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
