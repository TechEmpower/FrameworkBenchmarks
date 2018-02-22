FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -qqy software-properties-common build-essential curl locales wget git

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

ADD TFBReaper TFBReaper

RUN mv TFBReaper/target/debug/tfb_reaper /

ARG CPU_COUNT
ARG MAX_CONCURRENCY

ENV CPU_COUNT=$CPU_COUNT
ENV MAX_CONCURRENCY=$MAX_CONCURRENCY

ENTRYPOINT ["/tfb_reaper"]
