FROM maven:slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark_spring
RUN mvn -B clean package

FROM openjdk:slim
RUN apt-get update && apt-get install -y libjna-java procps
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_spring/target/woof_benchmark_spring-1.0.0-exec.jar server.jar
COPY start_server.sh start_server.sh
CMD ./start_server.sh

