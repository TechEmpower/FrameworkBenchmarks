FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /bayou
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /bayou
COPY --from=maven /bayou/target/bayou_TFB-0.1-jar-with-dependencies.jar app.jar
CMD ["java", "-jar", "app.jar"]
