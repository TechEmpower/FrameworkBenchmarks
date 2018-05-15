FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /blade
COPY package.xml package.xml
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "target/dist/hello-blade-1.0.0-BUILD-SNAPSHOT/hello-blade-1.0.0-BUILD-SNAPSHOT.jar"]
