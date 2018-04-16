FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /light-4j
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /light-4j
COPY --from=maven /light-4j/target/techempower-1.0.0.jar app.jar
CMD ["java", "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005", "-server", "-Xms512m", "-Xmx2g", "-jar", "app.jar"]
