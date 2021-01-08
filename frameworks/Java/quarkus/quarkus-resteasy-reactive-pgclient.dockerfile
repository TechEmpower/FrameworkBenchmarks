FROM maven:3.6.3-jdk-11-slim as maven
WORKDIR /quarkus
ENV MODULE=resteasy-reactive-pgclient

COPY pom.xml pom.xml
COPY $MODULE/pom.xml $MODULE/pom.xml

# Uncomment to test pre-release quarkus
#RUN mkdir -p /root/.m2/repository/io
#COPY m2-quarkus /root/.m2/repository/io/quarkus

WORKDIR /quarkus/$MODULE
RUN mvn dependency:go-offline -q
WORKDIR /quarkus

COPY $MODULE/src $MODULE/src

WORKDIR /quarkus/$MODULE
RUN mvn package -q
WORKDIR /quarkus

FROM openjdk:11.0.6-jdk-slim
WORKDIR /quarkus
ENV MODULE=resteasy-reactive-pgclient

COPY --from=maven /quarkus/$MODULE/target/lib lib
COPY --from=maven /quarkus/$MODULE/target/$MODULE-1.0-SNAPSHOT-runner.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:-UseBiasedLocking", "-XX:+UseStringDeduplication", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Djava.lang.Integer.IntegerCache.high=10000", "-Dvertx.disableHttpHeadersValidation=true", "-Dvertx.disableMetrics=true", "-Dvertx.disableH2c=true", "-Dvertx.disableWebsockets=true", "-Dvertx.flashPolicyHandler=false", "-Dvertx.threadChecks=false", "-Dvertx.disableContextTimings=true", "-Dvertx.disableTCCL=true", "-Dhibernate.allow_update_outside_transaction=true", "-Djboss.threads.eqe.statistics=false", "-jar", "app.jar"]
