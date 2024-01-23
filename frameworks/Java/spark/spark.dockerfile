FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /spark
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /spark
COPY --from=maven /spark/target/hello-spark-1.0.0-BUILD-SNAPSHOT.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
