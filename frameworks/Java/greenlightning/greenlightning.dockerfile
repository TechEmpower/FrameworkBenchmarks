FROM maven:3.5.3-jdk-10-slim as maven

WORKDIR /greenlightning    
COPY pom.xml pom.xml
COPY src src
RUN mvn install -q

FROM openjdk:10-jre-slim
WORKDIR /greenlightning
COPY --from=maven /greenlightning/target/greenlightning-test.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]
