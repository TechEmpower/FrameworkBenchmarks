FROM ubuntu:18.04

RUN apt-get update -yqq && \
	apt-get install -yqq cmake git uuid-dev gcc g++ autoconf

ENV IASIO=/asio/asio
ENV CINATRA=/cinatra/example

RUN git clone https://github.com/chriskohlhoff/asio.git

WORKDIR $IASIO

RUN ./autogen.sh && ./configure
RUN make -j && make install

WORKDIR /

RUN git clone https://github.com/qicosmos/cinatra.git

WORKDIR $CINATRA

RUN mkdir build && cd build && cmake .. && make

EXPOSE 8090

CMD ./build/cinatra_example
