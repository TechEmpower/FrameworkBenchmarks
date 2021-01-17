FROM maven:3.6.3 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark_spring
RUN mvn -B clean package

FROM openjdk:15
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_spring/target/woof_benchmark_spring-1.0.0-exec.jar server.jar
EXPOSE 8080
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dhttp.port=8080", "-Dhttp.server.name=O", "-Dhttp.date.header=true", "-jar", "server.jar"]
