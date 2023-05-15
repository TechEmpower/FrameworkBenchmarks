FROM registry.access.redhat.com/ubi8/openjdk-17:1.15 as maven
WORKDIR /quarkus
ENV MODULE=resteasy-reactive-hibernate-reactive

COPY pom.xml pom.xml
COPY quarkus-benchmark-common quarkus-benchmark-common/
COPY resteasy-hibernate resteasy-hibernate/
COPY resteasy-reactive-hibernate resteasy-reactive-hibernate/
COPY resteasy-reactive-hibernate-reactive resteasy-reactive-hibernate-reactive/

# Uncomment to test pre-release quarkus
#RUN mkdir -p /root/.m2/repository/io
#COPY m2-quarkus /root/.m2/repository/io/quarkus

WORKDIR /quarkus
RUN mvn -DskipTests install -pl :benchmark,:quarkus-benchmark-common -B -q

WORKDIR /quarkus/$MODULE
RUN mvn dependency:go-offline -B -q
WORKDIR /quarkus

COPY $MODULE/src $MODULE/src

WORKDIR /quarkus/$MODULE
RUN mvn package -B -q
WORKDIR /quarkus

FROM registry.access.redhat.com/ubi8/openjdk-17-runtime:1.15
WORKDIR /quarkus
ENV MODULE=resteasy-reactive-hibernate-reactive

COPY --from=maven /quarkus/$MODULE/target/quarkus-app/lib/ lib
COPY --from=maven /quarkus/$MODULE/target/quarkus-app/app/ app
COPY --from=maven /quarkus/$MODULE/target/quarkus-app/quarkus/ quarkus
COPY --from=maven /quarkus/$MODULE/target/quarkus-app/quarkus-run.jar quarkus-run.jar
COPY run_quarkus.sh run_quarkus.sh

EXPOSE 8080
ENTRYPOINT "./run_quarkus.sh"
