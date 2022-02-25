FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /tapestry
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM openjdk:11.0.3-jdk-stretch
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /tapestry/target/tapestry.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
