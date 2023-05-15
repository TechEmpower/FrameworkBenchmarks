FROM registry.access.redhat.com/ubi8/openjdk-17:1.15 as maven
ENV LANGUAGE='en_US:en'

WORKDIR /quarkus
ENV MODULE=resteasy-reactive-hibernate-reactive

COPY --chown=185 pom.xml pom.xml
COPY --chown=185 quarkus-benchmark-common quarkus-benchmark-common/
COPY --chown=185 resteasy-hibernate resteasy-hibernate/
COPY --chown=185 resteasy-reactive-hibernate resteasy-reactive-hibernate/
COPY --chown=185 resteasy-reactive-hibernate-reactive resteasy-reactive-hibernate-reactive/

# Uncomment to test pre-release quarkus
#RUN mkdir -p /root/.m2/repository/io
#COPY m2-quarkus /root/.m2/repository/io/quarkus

USER 185
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
ENV LANGUAGE='en_US:en'
WORKDIR /quarkus
ENV MODULE=resteasy-reactive-hibernate-reactive

COPY --chown=185 --from=maven /quarkus/$MODULE/target/quarkus-app/lib/ lib
COPY --chown=185 --from=maven /quarkus/$MODULE/target/quarkus-app/app/ app
COPY --chown=185 --from=maven /quarkus/$MODULE/target/quarkus-app/quarkus/ quarkus
COPY --chown=185 --from=maven /quarkus/$MODULE/target/quarkus-app/quarkus-run.jar quarkus-run.jar
COPY --chown=185 run_quarkus.sh run_quarkus.sh

EXPOSE 8080
USER 185
ENTRYPOINT "./run_quarkus.sh"
