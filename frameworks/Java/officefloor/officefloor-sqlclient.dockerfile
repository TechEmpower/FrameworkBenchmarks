FROM maven:3.6.3 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn -B -N clean install
WORKDIR /officefloor/src/woof_benchmark_woof
RUN mvn -B clean install
WORKDIR /officefloor/src/woof_benchmark_sqlclient
RUN mvn -B clean package

FROM openjdk:15-slim
RUN apt-get update && apt-get install -y libjna-java
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_sqlclient/target/woof_benchmark_sqlclient-1.0.0.jar server.jar
EXPOSE 8080
CMD ["java", "-Xms2g", "-Xmx2g", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dvertx.disableMetrics=true", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-jar", "server.jar"]
