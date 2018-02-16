FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -qqy software-properties-common build-essential curl locales

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

ADD TFBReaper TFBReaper

RUN mv TFBReaper/target/debug/tfb_reaper /

ENTRYPOINT ["/tfb_reaper"]