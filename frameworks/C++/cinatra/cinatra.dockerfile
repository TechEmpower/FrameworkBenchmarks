FROM ubuntu:22.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout 849f76710e25cc638bae18b597593e26e2966085
RUN mkdir build && cd build && cmake .. && make -j
EXPOSE 8090
CMD ./build/example/benchmark