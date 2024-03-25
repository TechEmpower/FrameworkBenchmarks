FROM ubuntu:23.10

ADD ./ /urweb
WORKDIR /urweb

RUN apt-get update -yqq && apt-get install -yqq sudo git gcc make autoconf automake libtool mlton libpq-dev libssl-dev uthash-dev libicu-dev && git clone https://github.com/urweb/urweb.git && cd urweb && ./autogen.sh && ./configure && make -j && sudo make install

RUN urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

EXPOSE 8080

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
