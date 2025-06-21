FROM maven:3-eclipse-temurin-24-alpine as maven
WORKDIR /voovan
COPY pom.xml pom.xml
COPY src src
COPY config/framework.properties config/framework.properties
RUN mvn package -q

FROM openjdk:25-ea-slim-bullseye
WORKDIR /voovan
COPY --from=maven /voovan/target/voovan-bench-0.1-jar-with-dependencies.jar app.jar
COPY --from=maven /voovan/config/framework.properties config/framework.properties

EXPOSE 8080

CMD java -DCheckTimeout=false \
    -DThreadBufferPoolSize=1024 \
    -DByteBufferSize=4096 \
    -DAsyncSend=false \
    -DAsyncRecive=false \
    -DByteBufferAnalysis=-1\
    -DServer=v \
    -server -Xms2g -Xmx2g \
    -XX:+DoEscapeAnalysis \
    -XX:+AlwaysPreTouch \
    -XX:-RestrictContended \
    -XX:+UseParallelGC -XX:+UseNUMA \
    --add-opens java.base/java.lang=ALL-UNNAMED \
    --add-opens java.base/java.util=ALL-UNNAMED \
    --add-opens java.base/java.io=ALL-UNNAMED \
    --add-opens java.base/java.nio=ALL-UNNAMED \
    --add-opens java.base/sun.nio.ch=ALL-UNNAMED \
    --add-opens java.base/java.security=ALL-UNNAMED \
    --add-opens java.base/java.util.concurrent=ALL-UNNAMED \
    --add-opens java.base/java.net=ALL-UNNAMED \
    -cp ./config:voovan.jar:app.jar org.voovan.VoovanTFB
