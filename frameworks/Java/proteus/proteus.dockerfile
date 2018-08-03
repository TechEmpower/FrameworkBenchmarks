FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /proteus
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q --update-snapshots

FROM openjdk:10-jre-slim
WORKDIR /proteus
COPY --from=maven /proteus/target/proteus-techempower-1.0.0.jar app.jar
COPY --from=maven /proteus/target/lib lib
COPY conf conf
CMD ["java", "-Dlogback.configurationFile=conf/logback.xml", "-Dconfig.file=conf/application.conf", "-Xms2g", "-Xmx2g", "-XX:+AggressiveOpts", "-server", "-XX:-UseBiasedLocking", "-XX:+UseStringDeduplication", "-Djava.net.preferIPv4Stack=true", "-classpath", "app.jar:lib/*", "io.sinistral.ExampleApplication"]
