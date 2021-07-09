FROM maven:3.6.3-openjdk-16-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

FROM ghcr.io/graalvm/graalvm-ce:latest
RUN gu install native-image
WORKDIR /redkale
COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

RUN native-image -jar redkale-benchmark.jar

RUN ls -lh

EXPOSE 8080

CMD ./redkale-benchmark &
