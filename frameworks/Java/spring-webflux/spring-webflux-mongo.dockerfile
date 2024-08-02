FROM maven:3.9.5-eclipse-temurin-21 as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM bellsoft/liberica-openjre-debian:21
WORKDIR /spring
COPY --from=maven /spring/target/spring-webflux-benchmark.jar app.jar
# See https://docs.spring.io/spring-boot/reference/packaging/efficient.html
RUN java -Djarmode=tools -jar app.jar extract


EXPOSE 8080

CMD ["java", "-Dlogging.level.root=OFF", "-Dreactor.netty.http.server.lastFlushWhenNoRead=true", "-jar", "app/app.jar", "--spring.profiles.active=mongo"]