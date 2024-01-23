FROM maven:3.8.2-openjdk-16 as maven
WORKDIR /inverno
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD [ "target/maven-inverno/application_linux_amd64/inverno-benchmark-1.0.0-SNAPSHOT/bin/inverno-benchmark", "--com.techempower.inverno.benchmark.appConfiguration.boot.reactor_prefer_vertx=false" ]
