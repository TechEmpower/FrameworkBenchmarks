FROM ubuntu:22.04
RUN apt-get update -yqq > /dev/null && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf > /dev/null
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout c9bec308e27174c8b7f0f01c92652509f7b47253
RUN mkdir build && cd build && cmake -DCMAKE_RULE_MESSAGES=OFF .. && make -j --silent
EXPOSE 8090
CMD ./build/example/benchmark
