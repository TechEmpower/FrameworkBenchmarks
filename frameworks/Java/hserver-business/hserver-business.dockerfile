FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /hserver-business
COPY pom.xml pom.xml
COPY src src
RUN mvn package --quiet

FROM openjdk:11.0.3-jdk-slim
WORKDIR /hserver-business
COPY --from=maven /hserver-business/target/hserver-business-1.0.jar app.jar

EXPOSE 8888

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dio.netty.buffer.checkBounds=false", "-Dio.netty.buffer.checkAccessible=false", "-Dio.netty.iouring.iosqeAsyncThreshold=32000", "-jar", "app.jar"]

