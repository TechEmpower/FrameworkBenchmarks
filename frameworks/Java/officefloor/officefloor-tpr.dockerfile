FROM maven:3.5.4-jdk-10 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_tpr
RUN mvn -q clean package

FROM openjdk:10
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_tpr/target/woof_tpr-1.0.0.jar server.jar
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dhttp.port=8080", "-Dhttp.server.name=OF", "-Dhttp.date.header=true", "-jar", "server.jar"]
