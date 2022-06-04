FROM ubuntu:16.04

ADD ./ /onion
WORKDIR /onion

RUN apt-get update -yqq && apt-get install -yqq git cmake make libmysqlclient-dev libjson0-dev libgnutls-dev libgcrypt-dev

ENV ONION_LOG noinfo

RUN git clone https://github.com/davidmoreno/onion.git && \
    cd onion && \
    git checkout f6b9d9e0689871226d671fb641698974e3f38762 && \
    mkdir -p build && \
    cd build && \
    cmake .. && \
    make

RUN ln -v onion/build/src/onion/libonion_static.a . && make

EXPOSE 8080

CMD ["./hello"]
