FROM openjdk:11-jdk

ARG SBT_VERSION=1.2.8

# Install sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update -yqq
RUN apt-get install -yqq sbt

WORKDIR /vertx
COPY src src
COPY project project
COPY build.sbt build.sbt
RUN sbt assembly

EXPOSE 8080

CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|tfb-database|$DBIP|g" /vertx/src/main/conf/config.json && \
    java \
      -Xms2G \
      -Xmx2G \
      -server \
      -Dvertx.disableMetrics=true \
      -Dvertx.disableH2c=true \
      -Dvertx.disableWebsockets=true \
      -Dvertx.flashPolicyHandler=false \
      -Dvertx.threadChecks=false \
      -Dvertx.disableContextTimings=true \
      -Dvertx.disableTCCL=true \
      -Dvertx.disableHttpHeadersValidation=true \
      -jar \
      target/scala-2.12/vertx-web-scala-assembly-1.jar \
      src/main/conf/config.json