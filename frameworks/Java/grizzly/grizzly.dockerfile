FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /grizzly
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /grizzly
COPY --from=maven /grizzly/target/grizzly-bm-0.1-jar-with-dependencies.jar app.jar
CMD ["java", "-Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384", "-Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024", "-Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32", "-jar", "app.jar"]
