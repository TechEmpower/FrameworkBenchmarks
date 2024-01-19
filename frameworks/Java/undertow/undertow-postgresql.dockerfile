FROM maven:3.8.6-openjdk-18 as maven
WORKDIR /undertow
COPY pom.xml .
COPY src src
RUN mvn package -q

FROM openjdk:18
WORKDIR /undertow
COPY --from=maven /undertow/target/lib lib
COPY --from=maven /undertow/target/app.jar .

EXPOSE 8080

CMD ["java", "-jar", "app.jar", "POSTGRESQL"]
