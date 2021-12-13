FROM yottadb/yottadb-base:latest

ARG DEBIAN_FRONTEND=noninteractive
RUN apt update && \
    apt upgrade -y && \
    apt install -y \
    libcurl4-openssl-dev \
    git

# Install cURL plugin
RUN git clone https://github.com/shabiel/fis-gtm-plugins.git
ENV LD_LIBRARY_PATH /opt/yottadb/current
RUN cd fis-gtm-plugins/libcurl && \
    . /opt/yottadb/current/ydb_env_set && \
    export gtm_dist=$ydb_dist && \
    make install

# Install M-Unit
RUN git clone https://github.com/ChristopherEdwards/M-Unit.git munit

RUN cd munit && \
    mkdir r && \
    cd Routines && \
    for file in _*.m; do mv $file /data/munit/r/; done

RUN git clone https://github.com/sumeetchhetri/M-Web-Server
RUN mkdir -p /mwebserver/r && cp -rf M-Web-Server/src/* /mwebserver/r

# Install M-Web-Server
COPY _techempbenchmark.m /mwebserver/r/

ENV GTMXC_libcurl "/opt/yottadb/current/plugin/libcurl_ydb_wrapper.xc"
RUN . /opt/yottadb/current/ydb_env_set && \
    export ydb_routines="/mwebserver/r /data/munit/r $ydb_routines" && \
    mumps -r ^%webtest

COPY run.sh ./
RUN chmod +x run.sh

EXPOSE 8080

CMD ./run.sh

