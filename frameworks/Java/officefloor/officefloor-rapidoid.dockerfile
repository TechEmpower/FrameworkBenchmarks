FROM maven:3.5.4-jdk-10 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark
RUN mvn clean install
WORKDIR /officefloor/src/woof_rapidoid
RUN mvn clean package

FROM openjdk:10
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_rapidoid/target/woof_rapidoid-1.0.0.jar server.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dhttp.port=8080", "-Dhttp.server.name=OF", "-Dhttp.date.header=true", "-jar", "server.jar"]
