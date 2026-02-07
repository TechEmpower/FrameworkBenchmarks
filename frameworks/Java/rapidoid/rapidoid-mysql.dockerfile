FROM maven:3-eclipse-temurin-25-alpine as maven
WORKDIR /rapidoid
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM amazoncorretto:25
WORKDIR /rapidoid
COPY --from=maven /rapidoid/target/rapidoid-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", \
     "-server", \
     "-XX:+UseParallelGC", \
     "-XX:MaxRAMPercentage=70", \
     "--add-opens", "java.base/java.lang=ALL-UNNAMED", \
     "--add-opens", "java.base/java.lang.reflect=ALL-UNNAMED", \
     "-cp", "app.jar", \
     "highlevel.Main", \
     "profiles=mysql,production"]
