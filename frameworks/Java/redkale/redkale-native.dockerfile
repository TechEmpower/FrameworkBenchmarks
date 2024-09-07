FROM maven:3.9.6-amazoncorretto-21-debian as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q


FROM ghcr.io/graalvm/native-image-community:22.0.2 as native
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar
RUN native-image -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar redkale-benchmark.jar

FROM ghcr.io/graalvm/jdk-community:22.0.2
WORKDIR /redkale
COPY --from=native /redkale/redkale-benchmark redkale-benchmark

EXPOSE 8080

CMD ./redkale-benchmark 
