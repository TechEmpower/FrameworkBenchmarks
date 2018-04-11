FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /activeweb
COPY pom.xml pom.xml
COPY scripts scripts
COPY src src
RUN mvn package -DskipTests -q

FROM openjdk:9-jdk
WORKDIR /resin
RUN curl -sL http://www.caucho.com/download/resin-4.0.55.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /activeweb/target/activeweb.war webapps/ROOT.war
CMD ["java", "-jar", "lib/resin.jar", "console"]
