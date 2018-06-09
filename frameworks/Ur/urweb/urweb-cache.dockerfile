FROM ubuntu:18.04

ADD ./ /urweb
WORKDIR /urweb

RUN apt update -yqq && apt install -yqq urweb

RUN urweb -sqlcache -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
