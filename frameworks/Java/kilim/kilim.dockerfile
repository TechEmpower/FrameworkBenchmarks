FROM maven:3.5.3-jdk-11-slim as maven
WORKDIR /db4j
COPY pom.xml pom.xml
RUN mvn dependency:copy-dependencies -DoutputDirectory=libs
COPY src src
RUN mvn install -q

FROM openjdk:11-jre-slim
WORKDIR /db4j
COPY --from=maven /db4j/libs libs
COPY --from=maven /db4j/target target
CMD java -cp target/classes:libs/\* hello.HelloController

