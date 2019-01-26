FROM alpine:3.8

RUN \
  apk add --no-cache --update bash wget

RUN \
  mkdir /opt && wget -qO- https://download.java.net/java/early_access/alpine/27/binaries/openjdk-11+27_linux-x64-musl_bin.tar.gz | tar xz -C /opt

ENV JAVA_HOME /opt/jdk-11/bin
ENV SCALA_VERSION 2.12.8
ENV SBT_VERSION 1.2.8

RUN \
  mkdir -p /usr/local/sbt && \
  wget -qO - --no-check-certificate "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | tar xz -C /usr/local/sbt --strip-components=1

ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${JAVA_HOME}/bin:${SBT_HOME}/bin

RUN \
  sbt sbtVersion && \
  rm -rf project & rm -rf target

RUN \
  wget -qO - --no-check-certificate "https://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz" | tar xfz - -C /root/ && \
  echo >> /root/.bashrc && \
  echo "export PATH=~/scala-$SCALA_VERSION/bin:$PATH" >> /root/.bashrc && \
  apk del wget

WORKDIR /akka-http-slick-postgres

COPY project project
COPY src src
COPY build.sbt .sbtopts .scalafmt.conf ./

RUN \
  hostname && \
  pwd && \
  ls -la && \
  sbt sbtVersion && \
  sbt -batch clean compile stage

CMD ["target/universal/stage/bin/akka-slick-benchmark", "-Dakka.http.benchmark.postgres.dbhost=tfb-database", "-J-d64", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:UseG1GC", "-J-XX:ParallelGCThreads=3", "-J-XX:MetaspaceSize=192M", "-J-XX:MaxMetaspaceSize=192M", "-J-XX:+UseStringDeduplication"]
