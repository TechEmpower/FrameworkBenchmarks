FROM gradle:4.7.0-jdk8 as gradle
USER root
WORKDIR /ratpack
COPY build.gradle build.gradle
COPY src src
RUN gradle shadowJar

FROM openjdk:8-jre-slim
WORKDIR /ratpack
COPY --from=gradle /ratpack/build/libs/ratpack-all.jar app.jar
CMD export DBIP=`getent hosts tfb-database | awk '{ print $1 }'` && \
    java \
        -server \
        -XX:+UseNUMA \
        -XX:+UseParallelGC \
        -Dvertx.disableMetrics=true \
        -Dvertx.disableH2c=true \
        -Dvertx.disableWebsockets=true \
        -Dvertx.flashPolicyHandler=false \
        -Dvertx.threadChecks=false \
        -Dvertx.disableContextTimings=true \
        -Dvertx.disableTCCL=true \
        -jar app.jar profile.name=pgclient database.host=$DBIP