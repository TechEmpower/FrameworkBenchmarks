FROM maven:3.6.1-jdk-8-slim as maven
WORKDIR /app

COPY pom.xml pom.xml
RUN mvn dependency:go-offline

COPY src src
RUN mvn package -Passembly -q

#TODO use separate JDK/JRE for the RUN (as the other builds)

#WORKDIR /app
#COPY target/tio-http-server-benchmark-1.0.jar tio-http-server-benchmark-1.0.jar

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dpacket.handler.mode=queue1", "-jar", "/app/target/tio-http-server-benchmark-1.0.jar"]