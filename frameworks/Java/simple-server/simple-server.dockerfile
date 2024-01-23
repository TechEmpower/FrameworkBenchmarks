FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /build
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
COPY --from=maven /build/target/appassembler /server

EXPOSE 8080

CMD ["/server/bin/main"]