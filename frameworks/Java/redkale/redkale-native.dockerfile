FROM maven:3.6.3-openjdk-16-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q
RUN ls -lh target/classes/META-INFO
RUN tail -n10000 target/classes/META-INF/redkale/redkale.load.classes
RUN tail -n10000 target/classes/META-INF/native-image/org.redkalex/redkale-benchmark/reflect-config.json

FROM ghcr.io/graalvm/graalvm-ce:latest
RUN gu install native-image
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

RUN native-image -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar redkale-benchmark.jar

RUN ls -lh

EXPOSE 8080

CMD ./redkale-benchmark 
