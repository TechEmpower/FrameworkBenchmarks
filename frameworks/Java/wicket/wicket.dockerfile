FROM maven:3-eclipse-temurin-24-alpine as maven
WORKDIR /wicket
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM alpine/curl:8.1.2 as curl
WORKDIR /wicket
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz >resin.tar.gz

FROM openjdk:25-ea-slim-bullseye
WORKDIR /resin
COPY --from=curl /wicket/resin.tar.gz .
RUN tar xzf ./resin.tar.gz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /wicket/target/hellowicket-1.0.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-XX:+UseParallelGC", "-jar", "lib/resin.jar", "console"]
