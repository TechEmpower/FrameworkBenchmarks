FROM sbtscala/scala-sbt:eclipse-temurin-17.0.5_8_1.8.2_2.12.17

ARG SBT_VERSION=1.8.2

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
