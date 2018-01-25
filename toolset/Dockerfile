FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -qqy software-properties-common build-essential

ADD setup/linux/TFBReaper.c TFBReaper.c

RUN gcc -std=c99 -oTFBReaper ./TFBReaper.c

ENTRYPOINT ["TFBReaper"]