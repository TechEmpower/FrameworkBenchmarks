FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

RUN apt-get update
RUN apt-get install -y build-essential \
  emacs-goodies-el \
  libgmp-dev \
  libssl-dev \
  libpq-dev \
  libsqlite3-dev \
  mlton \
  sqlite3 \
  libicu-dev

ADD https://github.com/urweb/urweb/releases/download/20200209/urweb-20200209.tar.gz /tmp/urweb.tgz
RUN tar xzf /tmp/urweb.tgz
RUN rm /tmp/urweb.tgz
RUN mv urweb* /urweb

ADD ./ /urweb
WORKDIR /urweb

RUN ./configure && \
    make && \
    make install

RUN urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

EXPOSE 8080

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
