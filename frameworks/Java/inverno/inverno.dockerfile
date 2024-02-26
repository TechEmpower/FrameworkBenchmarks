FROM maven:3.9.6-amazoncorretto-21 as maven
WORKDIR /inverno
COPY src src
COPY pom.xml pom.xml
RUN yum -y install binutils
RUN mvn package -q -Pio.inverno.io_uring

EXPOSE 8080

CMD [ "target/inverno-benchmark-1.0.0-SNAPSHOT-application_linux_amd64/bin/inverno-benchmark", "--com.techempower.inverno.benchmark.appConfiguration.boot.reactor_prefer_vertx=false" ]
