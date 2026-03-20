FROM maven:3.9-eclipse-temurin-25 AS build
WORKDIR /avaje-jex
COPY pom.xml pom.xml
RUN mvn dependency:go-offline -q
COPY src src
RUN mvn package -q
EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+UseCompactObjectHeaders", "-p", "./target/modules/", "-m", "avaje.techempower"]
