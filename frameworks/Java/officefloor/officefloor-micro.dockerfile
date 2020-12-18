FROM maven:slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark_micro
RUN mvn -B clean package

FROM openjdk:slim
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_micro/target/woof_benchmark_micro-1.0.0.jar server.jar
COPY start_server.sh start_server.sh
CMD ./start_server.sh

