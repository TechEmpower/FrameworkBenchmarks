FROM maven:3.5.4-jdk-8-slim as maven

WORKDIR /greenlightning    
COPY pom.xml pom.xml
COPY src src
RUN mvn clean install -q

#FROM openjdk:11-jdk-slim
FROM nimmis/java-centos:openjdk-8-jre-headless

WORKDIR /greenlightning
COPY --from=maven /greenlightning/target/greenlightning-test.jar app.jar
CMD ["java", "-server", "-Xmx14g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]
