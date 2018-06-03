FROM ubuntu:16.04

ADD ./ /urweb
WORKDIR /urweb

RUN apt update -yqq && apt install -yqq make wget libssl-dev libpq-dev libmysqlclient-dev urweb

RUN urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
