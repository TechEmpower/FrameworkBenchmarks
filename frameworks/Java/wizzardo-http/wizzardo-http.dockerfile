FROM gradle:4.7.0-jdk8

USER root
WORKDIR /wizzardo-http

COPY build.gradle build.gradle
COPY src src

RUN gradle --refresh-dependencies clean fatJar

CMD java \
    -Xmx2G \
    -Xms2G \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar build/libs/wizzardo-http-all-1.0-SNAPSHOT.jar env=prod
