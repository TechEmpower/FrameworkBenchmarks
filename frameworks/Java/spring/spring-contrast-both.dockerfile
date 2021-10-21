FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /spring
COPY --from=maven /spring/target/hello-spring-1.0-SNAPSHOT.jar app.jar

EXPOSE 8080

COPY contrast.jar contrast.jar
COPY contrast_security.yaml contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=true
ENV CONTRAST__PROTECT__ENABLE=true
ENV CONTRAST_CONFIG_PATH=contrast_security.yaml

CMD ["java", "-server", "-XX:MaxRAMPercentage=75", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=OFF", "-javaagent:contrast.jar", "-jar", "app.jar", "--spring.profiles.active=jdbc"]
