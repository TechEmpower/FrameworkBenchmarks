FROM buildpack-deps:bionic

RUN apt-get -yqq update
RUN apt-get -yqq install libgc-dev libunwind-dev

WORKDIR /opt

RUN wget -q https://github.com/dylan-lang/opendylan/releases/download/v2020.1.0/opendylan-2020.1-x86_64-linux.tar.bz2
RUN tar xjf opendylan-2020.1-x86_64-linux.tar.bz2

RUN git clone --recursive https://github.com/dylan-lang/http
RUN git clone https://github.com/dylan-lang/json

ENV PATH /opt/opendylan-2020.1/bin:$PATH

WORKDIR /

RUN make-dylan-app dylan-server

ENV OPEN_DYLAN_USER_REGISTRIES /dylan-server/registry:/opt/http/registry:/opt/json/registry

COPY *.dylan dylan-server/

WORKDIR /dylan-server

RUN dylan-compiler -build dylan-server.lid

EXPOSE 8080

CMD ./_build/bin/dylan-server
