FROM maven:3.9.9-eclipse-temurin-25-noble as maven
WORKDIR /solon
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM eclipse-temurin:25-jdk-noble
WORKDIR /solon
COPY --from=maven /solon/target/hello-solon.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseZGC", "-XX:+ZGenerational", "-cp", "app.jar", "hello.Main"]