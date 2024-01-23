FROM ubuntu:22.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout 79509a40cc8762965e32e8ee0f6e502d3aa2f0d6
RUN mkdir build && cd build && cmake .. && make -j
EXPOSE 8090
CMD ./build/example/benchmark