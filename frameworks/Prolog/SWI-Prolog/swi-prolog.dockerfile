FROM ubuntu:20.04

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN apt update -yqq && apt-get install -y software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable -y
RUN apt-get update -y && apt-get install -y --no-install-recommends swi-prolog swi-prolog-odbc odbc-postgresql

EXPOSE 8080

WORKDIR /app

COPY ./config/odbcinst.ini /etc/odbcinst.ini
COPY ./config/odbc.ini /etc/odbc.ini
COPY app .

RUN swipl --stand_alone=true \
          -g 'server(8080)' \
          -O \
          -o server \
          -c server.pl

CMD [ "/app/server", "--user=daemon", "--fork=false" ]
