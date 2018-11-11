FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-0.0.1.jar redkale-benchmark.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]