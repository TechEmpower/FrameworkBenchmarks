FROM maven:3.6.3-jdk-11-slim as maven
WORKDIR /quarkus
COPY pom.xml pom.xml
COPY base/pom.xml base/pom.xml
COPY hibernate/pom.xml hibernate/pom.xml
COPY pgclient/pom.xml pgclient/pom.xml
RUN mvn dependency:go-offline -q -pl base
COPY base/src/main/resources base/src/main/resources
COPY hibernate/src hibernate/src
COPY pgclient/src pgclient/src

RUN mvn package -q -pl pgclient -am

FROM openjdk:11.0.6-jdk-slim
WORKDIR /quarkus
COPY --from=maven /quarkus/pgclient/target/lib lib
COPY --from=maven /quarkus/pgclient/target/pgclient-1.0-SNAPSHOT-runner.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Djava.lang.Integer.IntegerCache.high=10000", "-Dvertx.disableHttpHeadersValidation=true", "-Dvertx.disableMetrics=true", "-Dvertx.disableH2c=true", "-Dvertx.disableWebsockets=true", "-Dvertx.flashPolicyHandler=false", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-Dhibernate.allow_update_outside_transaction=true", "-Djboss.threads.eqe.statistics=false", "-jar", "app.jar"]
