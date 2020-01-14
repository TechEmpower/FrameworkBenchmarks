FROM gradle:5.4.1-jdk11 as gradle
USER root
WORKDIR /javalin
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
    -jar build/libs/javalin-all-1.0-SNAPSHOT.jar env=prod
