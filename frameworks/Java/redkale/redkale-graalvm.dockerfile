FROM maven:3.6.3-openjdk-16-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q


FROM ubuntu:20.10
WORKDIR /redkale
RUN apt-get update -yqq
RUN apt-get install -yqq wget
RUN wget https://redkale.org/graalvm-ee-java16-linux-amd64-21.1.0.tar.gz
RUN tar -xvzf graalvm-ee-java16-linux-amd64-21.1.0.tar.gz
ENV JAVA_HOME /redkale/graalvm-ee-java16-linux-amd64-21.1.0
ENV PATH /redkale/graalvm-ee-java16-linux-amd64-21.1.0/bin:$PATH

COPY conf conf
COPY --from=maven /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=40000", "-Dbenchmarks.cache=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
