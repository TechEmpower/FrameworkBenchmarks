FROM ubuntu:20.10

RUN apt-get update -yqq && \
	apt-get install -yqq \
		git pkg-config build-essential cmake zlib1g-dev \
		libsqlite3-dev libmariadbclient-dev wget

ADD ./ /lwan
WORKDIR /lwan

RUN mkdir mimalloc && \
    wget https://github.com/microsoft/mimalloc/archive/6e1ca96a4965c776c10698c24dae576523178ef5.tar.gz -O - | tar xz --strip-components=1 -C mimalloc && \
    cd mimalloc && mkdir build && cd build && \
    CFLAGS="-flto -ffat-lto-objects" cmake .. -DCMAKE_BUILD_TYPE=Release -DMI_SECURE=OFF && make -j install

RUN mkdir luajit && \
    wget http://luajit.org/download/LuaJIT-2.0.5.tar.gz -O - | tar xz --strip-components=1 -C luajit && \
    cd luajit && \
    PREFIX=/usr CFLAGS="-O3 -mtune=native -march=native -flto -ffat-lto-objects" make -j install

RUN wget https://github.com/lpereira/lwan/archive/3bb25e235e76028d7687ae4636a059474f42b19c.tar.gz -O - | tar xz --strip-components=1 && \
    mkdir build && cd build && \
    cmake /lwan -DCMAKE_BUILD_TYPE=Release -DUSE_ALTERNATIVE_MALLOC=mimalloc && \
    make lwan-static

RUN make clean && make

ENV LD_LIBRARY_PATH=/usr/local/lib:/usr/lib
ENV USE_MYSQL=1
ENV MYSQL_USER=benchmarkdbuser
ENV MYSQL_PASS=benchmarkdbpass
ENV MYSQL_DB=hello_world
ENV MYSQL_HOST=tfb-database
ENV LD_PRELOAD=/usr/local/lib/mimalloc-1.6/libmimalloc.so

EXPOSE 8080

CMD ["./techempower"]
