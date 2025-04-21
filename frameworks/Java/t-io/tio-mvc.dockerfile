FROM maven:3.8.4-openjdk-17-slim as maven
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
COPY script script
RUN mvn package -q

FROM openjdk:17.0.2
WORKDIR /t-io/target/tio-http-server-benchmark

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-cp", "/t-io/target/tio-http-server-benchmark/config:/t-io/target/tio-http-server-benchmark/lib/*", "org.tio.http.server.benchmark.TioBenchmarkStarter"]


