FROM maven:3.9.0-eclipse-temurin-17
WORKDIR /jooby
COPY pom.xml pom.xml
COPY src src
COPY public public
COPY conf conf
RUN mvn package -q -P undertow

EXPOSE 8080

CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-cp", "target/jooby.jar", "com.techempower.MvcApp"]
