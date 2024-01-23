FROM maven:3-openjdk-16 AS builder
WORKDIR /runner
COPY pom.xml pom.xml
RUN mvn dependency:go-offline -B
COPY src src
RUN mvn package -q

FROM adoptopenjdk:16-jre-hotspot
COPY --from=builder /runner/target/tuprolog-web-runner-1.0-SNAPSHOT-jar-with-dependencies.jar /runner.jar
COPY app /app
EXPOSE 8080

CMD [ "java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "/runner.jar", "/app/application.prolog" ]
