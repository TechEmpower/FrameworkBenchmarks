FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /voovan
COPY pom.xml pom.xml
COPY src src
COPY config/framework.properties config/framework.properties
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /voovan
COPY --from=maven /voovan/target/voovan-bench-0.1-jar-with-dependencies.jar app.jar
COPY --from=maven /voovan/config/framework.properties config/framework.properties
CMD java -DCheckReadTimeout=false -DThreadBufferPoolSize=1024 -DAsyncSend=false \
    -server -Xms2g -Xmx2g \
    -XX:-RestrictContended \
    -XX:+UseParallelGC -XX:+UseNUMA \
    -XX:+AggressiveOpts -XX:+UseBiasedLocking \
    -cp ./config:voovan.jar:app.jar org.voovan.VoovanTFB
