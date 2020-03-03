FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn -q -N clean install
WORKDIR /officefloor/src/woof_benchmark
RUN mvn -q clean install
WORKDIR /officefloor/src/woof_benchmark_netty
RUN mvn -q clean package

FROM openjdk:11.0.3-jdk-slim
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_netty/target/woof_benchmark_netty-1.0.0.jar server.jar
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-Dhttp.port=8080", "-Dhttp.server.name=OF", "-Dhttp.date.header=true", "-jar", "server.jar"]
