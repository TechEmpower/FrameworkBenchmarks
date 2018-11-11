FROM ubuntu:16.04

ADD ./ /onion
WORKDIR /onion

RUN apt update -yqq && apt install -yqq git cmake make libmysqlclient-dev libjson0-dev libgnutls-dev libgcrypt-dev

ENV ONION_LOG noinfo

# Latest commit on master as of July 10 2014
# This is post tag v0.7, but pre any later tags

RUN git clone https://github.com/davidmoreno/onion.git && \
    cd onion && \
    git checkout c460557bfc7d45fb6ba61cb6b7259480a67dde82 && \
    mkdir -p build && \
    cd build && \
    cmake .. && \
    make

RUN make clean && \
    rm -f onion/build/CMakeCache.txt && \
    make

CMD ["./hello"]
