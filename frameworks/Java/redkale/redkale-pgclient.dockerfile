FROM maven:3.9.6-amazoncorretto-21-debian as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom-pgclient.xml pom.xml
RUN mvn package -q

FROM openjdk:23-jdk-slim
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dio.netty.buffer.checkBounds=false", "-Dio.netty.buffer.checkAccessible=false", "-Dvertx.disableURIValidation=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]