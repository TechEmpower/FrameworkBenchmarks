FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:10-jre-slim

CMD ["java", "-server", "-Xms256M", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dpacket.handler.mode=queue1", "-cp", "/t-io/target/tio-http-server-benchmark/config:/t-io/target/tio-http-server-benchmark/lib/*", "org.tio.http.server.benchmark.TioBenchmarkStarter"]