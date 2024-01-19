FROM ubuntu:22.04

RUN apt-get update -yqq && \
	apt-get install -yqq \
		git pkg-config build-essential cmake zlib1g-dev \
		libsqlite3-dev libmariadb-dev wget libluajit-5.1-dev

ADD ./ /lwan
WORKDIR /lwan

RUN mkdir mimalloc && \
    wget https://github.com/microsoft/mimalloc/archive/817569dfad79732233fb86649c89e04387ce02e9.tar.gz -O - | tar xz --strip-components=1 -C mimalloc && \
    cd mimalloc && mkdir build && cd build && \
    CFLAGS="-flto -ffat-lto-objects" cmake .. -DCMAKE_BUILD_TYPE=Release -DMI_SECURE=OFF && make -j install

RUN wget https://github.com/lpereira/lwan/archive/e637f1ea724389a36dcab02affb6ec3fe5ecb0b6.tar.gz -O - | tar xz --strip-components=1 && \
    mkdir build && cd build && \
    cmake /lwan -DCMAKE_BUILD_TYPE=Release -DUSE_ALTERNATIVE_MALLOC=mimalloc && \
    make lwan-static

RUN make clean && make

ENV LD_LIBRARY_PATH=/usr/local/lib:/usr/lib
ENV LD_PRELOAD=/usr/local/lib/libmimalloc.so

EXPOSE 8080

CMD ["./techempower"]
