FROM maven:3.9.6-amazoncorretto-21-debian as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q


FROM ghcr.io/graalvm/graalvm-ce:22.3.3
RUN gu install native-image
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

RUN native-image -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar redkale-benchmark.jar

RUN ls -lh

EXPOSE 8080

CMD ./redkale-benchmark 
