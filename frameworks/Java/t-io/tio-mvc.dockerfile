FROM maven:3.5.3-jdk-8
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
COPY script script
RUN mvn package -q
WORKDIR /t-io/target/tio-http-server-benchmark
CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dpacket.handler.mode=queue1", "-cp", "/t-io/target/tio-http-server-benchmark/config:/t-io/target/tio-http-server-benchmark/lib/*", "org.tio.http.server.benchmark.TioBenchmarkStarter"]