FROM techempower/base:0.2

COPY ./ ./

ENV URWEB_VERSION=20160621
ENV COMPILER=/urweb-build

RUN apt install -yqq mlton libssl-dev libpq-dev libmysqlclient-dev
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
