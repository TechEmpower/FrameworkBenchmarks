FROM maven:3.5.3-jdk-10-slim
WORKDIR /vertx
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q
CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    sed -i "s|tfb-database|$DBIP|g" /vertx/src/main/conf/config.json && \
    java \
      -Xms2G \
      -Xmx2G \
      -server \
      -XX:+UseNUMA \
      -XX:+UseParallelGC \
      -XX:+AggressiveOpts \
      -Dvertx.disableMetrics=true \
      -Dvertx.disableH2c=true \
      -Dvertx.disableWebsockets=true \
      -Dvertx.flashPolicyHandler=false \
      -Dvertx.threadChecks=false \
      -Dvertx.disableContextTimings=true \
      -Dvertx.disableTCCL=true \
      -jar \
      target/vertx.benchmark-0.0.1-SNAPSHOT-fat.jar \
      src/main/conf/config.json
