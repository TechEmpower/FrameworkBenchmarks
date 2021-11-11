FROM openjdk:8-jdk
WORKDIR /hot
COPY shows shows
COPY config-mysql.json config.json
ENV HOT_VERSION 0.9.15-SNAPSHOT
RUN curl -sL https://github.com/dsolimando/Hot/releases/download/${HOT_VERSION}/hot-${HOT_VERSION}.tar.gz | tar xz
ENV HOT_HOME /hot/hot-${HOT_VERSION}
ENV PATH ${HOT_HOME}:${PATH}

EXPOSE 8080

CMD ["hot", "run"]
