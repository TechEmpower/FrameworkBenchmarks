FROM maven:3.9-eclipse-temurin-25 AS build
WORKDIR /avaje-jex
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q
EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+UseCompactObjectHeaders", "-p", "./target/modules/", "-m", "avaje.techempower"]
