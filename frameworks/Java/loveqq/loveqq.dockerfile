FROM maven:3.9.7-amazoncorretto-21 as maven
WORKDIR /loveqq
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q -P !default,!dev,!gpg

FROM bellsoft/liberica-openjre-debian:23
WORKDIR /loveqq
COPY --from=maven /loveqq/target/boot-lib boot-lib
COPY --from=maven /loveqq/target/loveqq-benchmark-1.0-SNAPSHOT.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "--add-opens=java.base/java.lang=ALL-UNNAMED", "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED", "--add-opens=java.base/sun.reflect.annotation=ALL-UNNAMED", "-jar", "app.jar"]