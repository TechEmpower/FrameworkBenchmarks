FROM maven:3.6.1-jdk-8-slim as maven
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
COPY script script
RUN mvn package -Passembly -q

#TODO use separate JDK/JRE for the RUN (as the other builds)
WORKDIR /t-io/target/tio-server-benchmark

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dpacket.handler.mode=queue1", "-jar", "/t-io/target/tio-server-benchmark-1.0.jar"]
