FROM ubuntu:18.04
RUN apt-get update -yqq && \
apt-get install -yqq cmake git uuid-dev gcc g++ autoconf
ENV ASIO=/asio
ENV ASIO_INTERNAL=/asio/asio
ENV CINATRA=/cinatra
ENV CINATRA_EXAMPLE=/cinatra/example
WORKDIR /
RUN git clone https://github.com/chriskohlhoff/asio.git
WORKDIR $ASIO
RUN git checkout 8087252a0c3c2f0baad96ddbd6554db17a846376
WORKDIR $ASIO_INTERNAL
RUN ./autogen.sh && ./configure
RUN make && make install
WORKDIR /
RUN git clone https://github.com/qicosmos/cinatra.git
WORKDIR $CINATRA
RUN git checkout 9f12b7ddef1b919167348d49b79162f3c46d7fef
WORKDIR $CINATRA_EXAMPLE
RUN mkdir build && cd build && cmake .. && make
EXPOSE 8090
CMD ./build/cinatra_example
