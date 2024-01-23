FROM ubuntu:22.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout 3f50010b27a350261fd1ead4602fa1cb3c9a9598
RUN mkdir build && cd build && cmake .. && make
EXPOSE 8090
CMD ./build/example/benchmark