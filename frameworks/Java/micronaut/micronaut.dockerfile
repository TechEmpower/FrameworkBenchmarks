FROM adoptopenjdk/maven-openjdk11:latest as maven
WORKDIR /micronaut
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM adoptopenjdk/openjdk11:jdk-11.28-alpine-slim
WORKDIR /micronaut
COPY --from=maven /micronaut/target/hello-micronaut-0.1.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dmicronaut.environments=benchmark", "-Dlog-root-level=OFF", "-jar", "app.jar"]