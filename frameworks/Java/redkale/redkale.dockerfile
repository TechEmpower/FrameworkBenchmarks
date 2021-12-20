FROM maven:3.8.4-openjdk-17-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseZGC", "-XX:+UseLargePages", "-XX:SoftMaxHeapSize=18g", "-DAPP_HOME=./", "-jar", "/redkale/target/redkale-benchmark-1.0.0.jar"]