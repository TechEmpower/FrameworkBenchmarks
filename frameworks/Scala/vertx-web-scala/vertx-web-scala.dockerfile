FROM sbtscala/scala-sbt:eclipse-temurin-25.0.1_8_1.11.7_3.7.4

ARG SBT_VERSION=1.11.7

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
      target/scala-3.7.4/vertx-web-scala-assembly-1.jar \
      src/main/conf/config.json
