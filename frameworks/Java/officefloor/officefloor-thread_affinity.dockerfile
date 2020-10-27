FROM maven:slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn -q -N clean install
WORKDIR /officefloor/src/woof_benchmark_micro
RUN mvn -q clean install
WORKDIR /officefloor/src/woof_benchmark_thread_affinity
RUN mvn -q clean package

FROM openjdk:slim
RUN apt-get update && apt-get install -y libjna-java
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_thread_affinity/target/woof_benchmark_thread_affinity-1.0.0.jar server.jar
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-Dhttp.port=8080", "-Dhttp.server.name=OF", "-Dhttp.date.header=true", "-jar", "server.jar"]
