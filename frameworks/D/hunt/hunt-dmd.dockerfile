FROM dlang2/dmd-ubuntu:2.085.1

RUN apt-get update && apt-get install -y --no-install-recommends git && apt install -yqq libpq-dev libsqlite3-dev libmysqlclient-dev zlib1g-dev && rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/*

ADD ./ /hunt
WORKDIR /hunt

RUN git clone https://github.com/h2o/picohttpparser.git && \
    cp patches/Makefile picohttpparser \
    cd picohttpparser && \
    make package

RUN dub upgrade --verbose
RUN dub build -f --arch=x86_64 --build=release --compiler=dmd -c=lite

CMD ["./hunt-minihttp"]
