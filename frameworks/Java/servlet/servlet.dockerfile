FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /servlet
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM openjdk:10-jdk
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.56.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /servlet/target/servlet.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
