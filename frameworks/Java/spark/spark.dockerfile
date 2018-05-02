FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /spark
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:8-jdk
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.56.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /spark/target/spark.war webapps/ROOT.war
COPY resin.xml conf/resin.xml
CMD ["java", "-jar", "lib/resin.jar", "console"]
