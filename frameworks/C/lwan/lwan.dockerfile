FROM ubuntu:19.10

RUN apt update -yqq
RUN apt install -yqq \
	git pkg-config build-essential cmake zlib1g-dev \
	libsqlite3-dev libmariadbclient-dev wget

ADD ./ /lwan
WORKDIR /lwan

RUN mkdir mimalloc && \
    wget https://github.com/microsoft/mimalloc/archive/acb03c54971c4b0a43a6d17ea55a9d5feb88972f.tar.gz -O - | tar xz --strip-components=1 -C mimalloc && \
    cd mimalloc && mkdir build && cd build && CFLAGS="-flto -ffat-lto-objects" cmake .. -DCMAKE_BUILD_TYPE=Release -DMI_SECURE=OFF && make -j install && cd ../.. && \
    wget https://github.com/lpereira/lwan/archive/e3c03b6a975a7206a1b6e5d78a99abf5e7be1647.tar.gz -O - | tar xz --strip-components=1 && \
    mkdir build && cd build && \
    cmake /lwan -DCMAKE_BUILD_TYPE=Release -DUSE_ALTERNATIVE_MALLOC=mimalloc && \
    make lwan-static

RUN make clean && make

ENV USE_MYSQL=1
ENV MYSQL_USER=benchmarkdbuser
ENV MYSQL_PASS=benchmarkdbpass
ENV MYSQL_DB=hello_world
ENV MYSQL_HOST=tfb-database

CMD ["./techempower"]
