FROM maven:3.6.3 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn -B -N clean install
WORKDIR /officefloor/src/woof_benchmark_micro
RUN mvn -B clean install
WORKDIR /officefloor/src/woof_benchmark_thread_affinity
RUN mvn -B clean package

FROM openjdk:15-slim
RUN apt-get update && apt-get install -y libjna-java
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_thread_affinity/target/woof_benchmark_thread_affinity-1.0.0.jar server.jar
EXPOSE 8080
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dhttp.port=8080", "-Dhttp.server.name=O", "-Dhttp.date.header=true", "-jar", "server.jar"]
