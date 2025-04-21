FROM maven:3.9.7-amazoncorretto-21 as maven
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
COPY script script
RUN mvn clean package -q

FROM openjdk:21-jdk-slim
WORKDIR /t-io/target/tio-http-server-benchmark

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx4G", "-cp", "/t-io/target/tio-http-server-benchmark/config:/t-io/target/tio-http-server-benchmark.jar", "org.tio.http.server.benchmark.TioBenchmarkStarter"]


