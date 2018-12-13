FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /baseio
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /baseio
COPY --from=maven /baseio/target/baseio-example-0.1-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-Dlite=false", "-Dcore=1", "-Dframe=16", "-DreadBuf=512", "-Dpool=true", "-Ddirect=true", "-Dlevel=1", "-Dread=false", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]