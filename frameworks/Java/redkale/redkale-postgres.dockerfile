FROM maven:3.5.3-jdk-10-slim
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q
CMD ["java", "-DAPP_HOME=./", "-jar", "target/redkale-benchmark-0.0.1.jar"]