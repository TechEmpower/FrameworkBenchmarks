FROM dlangchina/dlang-dmd:latest

ADD ./ /hunt
WORKDIR /hunt

RUN apt update -y && apt install -y --no-install-recommends git && apt install -yqq libpq-dev libsqlite3-dev default-libmysqlclient-dev zlib1g-dev && rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/*

RUN git clone https://github.com/h2o/picohttpparser.git && \
    cp -rf patches/Makefile picohttpparser && \
    cd picohttpparser && \
    make package && \
    cd ..

RUN dub upgrade --verbose
RUN dub build --build=release --arch=x86_64 --compiler=dmd  -c=mmap -f

CMD ["./hunt-minihttp"]
