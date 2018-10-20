FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /greenlightning
COPY pom.xml pom.xml
COPY src src
<<<<<<< HEAD
RUN mvn install -q 
=======
RUN mvn clean install -q -U
>>>>>>> branch 'master' of https://github.com/oci-pronghorn/FrameworkBenchmarks.git

FROM openjdk:10-jre-slim
WORKDIR /greenlightning
COPY --from=maven /greenlightning/target/greenlightning-test.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]
