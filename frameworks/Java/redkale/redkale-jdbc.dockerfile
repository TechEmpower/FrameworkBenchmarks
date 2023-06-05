FROM maven:3.8.6-openjdk-18-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom-jdbc.xml pom.xml
RUN mvn package -q

FROM openjdk:20-jdk-slim
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "--enable-preview", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]