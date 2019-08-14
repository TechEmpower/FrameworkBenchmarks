FROM buildpack-deps:bionic

RUN apt-get -yqq update
RUN apt-get -yqq install libgc-dev libunwind-dev

WORKDIR /opt

RUN wget -q https://opendylan.org/downloads/opendylan/2019.1/opendylan-2019.1-x86_64-linux.tar.bz2
RUN tar xjf opendylan-2019.1-x86_64-linux.tar.bz2

RUN git clone --recursive https://github.com/dylan-lang/http

ENV PATH /opt/opendylan-2019.1/bin:$PATH

WORKDIR /

RUN make-dylan-app dylan-server

ENV OPEN_DYLAN_USER_REGISTRIES /dylan-server/registry:/opt/http/registry

COPY *.dylan dylan-server/

WORKDIR /dylan-server

RUN dylan-compiler -build dylan-server.lid

CMD ./_build/bin/dylan-server
