FROM ubuntu:19.04

RUN apt update -yqq
RUN apt install -yqq \
	git pkg-config build-essential cmake zlib1g-dev \
	libsqlite3-dev libmariadbclient-dev wget

ADD ./ /lwan
WORKDIR /lwan

RUN wget https://github.com/lpereira/lwan/archive/d7fc0d27fbea5c68d61444033517d0e962e822e6.tar.gz -O - | tar xz --strip-components=1 && \
    mkdir build && cd build && \
    cmake /lwan -DCMAKE_BUILD_TYPE=Release && \
    make lwan-static

RUN make clean && make

ENV USE_MYSQL=1
ENV MYSQL_USER=benchmarkdbuser
ENV MYSQL_PASS=benchmarkdbpass
ENV MYSQL_DB=hello_world
ENV MYSQL_HOST=tfb-database

CMD ["./techempower"]
