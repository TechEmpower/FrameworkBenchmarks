FROM maven:3.9.6-amazoncorretto-21-debian as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
COPY application-noshare.xml conf/application.xml
RUN mvn package -q

FROM openjdk:23-jdk-slim
WORKDIR /redkale
COPY conf conf
COPY application-noshare.xml conf/application.xml
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-XX:+UseParallelGC", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]