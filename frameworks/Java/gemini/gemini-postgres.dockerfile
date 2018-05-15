FROM maven:3.5.3-jdk-10-slim as maven

WORKDIR /gemini

COPY src src
COPY pom.xml pom.xml

RUN mvn -q compile
RUN mv src/main/webapp/WEB-INF/gemini-postgres.conf src/main/webapp/WEB-INF/GeminiHello.conf
RUN mvn -q war:war

FROM openjdk:10-jdk
RUN apt update -qqy && apt install -yqq curl > /dev/null

WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.56.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /gemini/target/HelloWorld-0.0.1.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
