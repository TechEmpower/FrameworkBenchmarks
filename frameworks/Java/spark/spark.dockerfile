FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /spark
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:8-jdk
WORKDIR /spark
COPY --from=maven /spark/target/hello-spark-1.0.0-BUILD-SNAPSHOT.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
