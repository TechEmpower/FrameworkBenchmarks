FROM maven:3.9.6-amazoncorretto-21-debian as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom-jdbc.xml pom.xml
RUN mvn package -q

FROM azul/zulu-openjdk:25
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+UseCompactObjectHeaders", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]