FROM ubuntu:20.04

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN apt update -yqq && apt-get install -y software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable -y
RUN apt-get update -y && apt-get install -y --no-install-recommends swi-prolog swi-prolog-odbc odbc-postgresql
RUN swipl -g 'pack_install(simple_template, [interactive(false), silent(true)]).'

EXPOSE 8080

WORKDIR /app

CMD [ "swipl", "server.pl", "--user=daemon", "--fork=false", "--port=8080" ]

COPY ./config/odbc.ini /etc/odbc.ini
COPY app .