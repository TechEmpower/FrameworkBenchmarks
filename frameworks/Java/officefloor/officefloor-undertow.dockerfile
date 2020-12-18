FROM maven:slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn -B -N clean install
WORKDIR /officefloor/src/woof_benchmark
RUN mvn -B clean install
WORKDIR /officefloor/src/woof_benchmark_undertow
RUN mvn -B clean package

FROM openjdk:slim
RUN apt-get update && apt-get install -y libjna-java procps
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_undertow/target/woof_benchmark_undertow-1.0.0.jar server.jar
COPY start_server.sh start_server.sh
CMD ./start_server.sh

