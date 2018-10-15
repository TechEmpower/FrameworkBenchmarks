FROM gradle:4.7.0-jdk8
USER root
WORKDIR /javalin
COPY build.gradle build.gradle
COPY src src
# RUN gradle build
RUN gradle --refresh-dependencies clean fatJar

CMD java \
    -Xmx2G \
    -Xms2G \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar build/libs/javalin-all-1.0-SNAPSHOT.jar env=prod