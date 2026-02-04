FROM maven:3-eclipse-temurin-25-alpine as maven
WORKDIR /httpserver-robaho
COPY pom.xml pom.xml
COPY src src
RUN mvn compile -P robaho assembly:single -q

FROM ibm-semeru-runtimes:open-25-jre-jammy
WORKDIR /httpserver-robaho
COPY --from=maven /httpserver-robaho/target/httpserver-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-Xtune:throughput", "-Xgcpolicy:optthruput", "-XX:MaxRAMPercentage=70", "-XX:+UseContainerSupport", "-Dclojure.compiler.direct-linking=true", "-Drobaho.net.httpserver.nodelay=true", "-jar", "app.jar"]
