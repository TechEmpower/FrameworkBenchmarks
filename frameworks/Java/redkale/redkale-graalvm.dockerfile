FROM maven:3.8.6-openjdk-18-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q


FROM ghcr.io/graalvm/jdk:java17-22.3.0
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=40000", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
