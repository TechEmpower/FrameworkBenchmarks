FROM ubuntu:22.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV CINATRA=/cinatra
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout 334669d25e3612a61cfcf0bd8ed0de3431f9ca5e
RUN mkdir build && cd build && cmake .. && make
EXPOSE 8090
CMD ./build/example/benchmark