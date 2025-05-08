FROM maven:3.9.9-eclipse-temurin-24 AS build
WORKDIR /avaje-jex
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q -P jetty
EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-p", "./target/modules/", "-m", "avaje.techempower"]
