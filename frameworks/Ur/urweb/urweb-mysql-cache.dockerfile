FROM ubuntu:18.04

ADD ./ /urweb
WORKDIR /urweb

RUN apt-get update -yqq && apt-get install -yqq urweb

RUN urweb -sqlcache -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
