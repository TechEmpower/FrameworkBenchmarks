FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /grizzly
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /grizzly
COPY --from=maven /grizzly/target/grizzly-bm-0.1-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384", "-Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024", "-Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32", "-jar", "app.jar"]
