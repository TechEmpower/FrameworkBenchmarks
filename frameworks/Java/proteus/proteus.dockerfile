FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /proteus
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q --update-snapshots

FROM openjdk:11.0.3-jdk-slim
WORKDIR /proteus
COPY --from=maven /proteus/target/proteus-techempower-1.0.0.jar app.jar
COPY --from=maven /proteus/target/lib lib
COPY conf conf

EXPOSE 8080

CMD ["java", "-Dlogback.configurationFile=conf/logback.xml", "-Dconfig.file=conf/application.conf", "-Xms4g", "-Xmx4g", "-XX:+AggressiveOpts", "-server", "-XX:-UseBiasedLocking", "-XX:+UseStringDeduplication", "-Djava.net.preferIPv4Stack=true", "-XX:+UseNUMA", "-classpath", "app.jar:lib/*", "io.sinistral.ExampleApplication"]
