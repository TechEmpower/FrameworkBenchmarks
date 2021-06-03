FROM ubuntu:20.04

ENV IROOT=/installs

ENV DEBIAN_FRONTEND noninteractive
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections

RUN apt update -yqq && apt-get install -y software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable -y
RUN apt-get update -y && apt-get install -y --no-install-recommends swi-prolog

COPY server.pl ${IROOT}/
WORKDIR ${IROOT}

EXPOSE 8080

CMD swipl server.pl --user=daemon --fork=false --port=8080

