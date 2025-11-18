FROM maven:3.9.9-eclipse-temurin-24-noble as maven
WORKDIR /jooby
COPY pom.xml pom.xml
COPY src src
COPY public public
COPY conf conf

RUN mvn package -q -P undertow

EXPOSE 8080

CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/jooby.jar"]
