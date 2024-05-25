FROM litongjava/maven:3.8.8-graalvm-jdk-21-slim
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
RUN mvn package -Pnative -q

EXPOSE 8080

CMD ["/t-io/target/tio-server-benchmark", " --native=true"]
