FROM ubuntu:16.04

RUN apt-get update

# Install some common development tools
RUN apt-get install -y software-properties-common build-essential curl locales wget unzip git \
    libmysqlclient-dev libpq-dev \
    libpcre3 libpcre3-dev \
    libssl-dev libcurl4-openssl-dev \
    zlib1g-dev \
    libreadline6-dev \
    libbz2-dev \
    libxslt-dev libgdbm-dev ncurses-dev  \
    libffi-dev libtool bison libevent-dev \
    libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
    liborc-0.4-0 libgnutls-dev \
    libjson0-dev libmcrypt-dev libicu-dev \
    re2c libnuma-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

ARG CPU_COUNT
ARG MAX_CONCURRENCY

ENV CPU_COUNT=$CPU_COUNT
ENV MAX_CONCURRENCY=$MAX_CONCURRENCY
