FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /wicket
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM openjdk:11.0.3-jdk-stretch
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /wicket/target/hellowicket-1.0.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "lib/resin.jar", "console"]
