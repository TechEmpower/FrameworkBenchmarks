FROM ubuntu:22.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout c9bec308e27174c8b7f0f01c92652509f7b47253
RUN mkdir build && cd build && cmake .. && make -j
EXPOSE 8090
CMD ./build/example/benchmark