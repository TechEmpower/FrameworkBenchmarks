FROM ubuntu:20.10
ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /redkale
RUN apt-get update -yqq
RUN apt-get install -yqq wget

RUN wget --no-verbose https://redkale.org/graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
RUN tar -xzf graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
ENV JAVA_HOME /redkale/graalvm-ee-java16-21.2.0

RUN wget --no-verbose https://ftp.wayne.edu/apache/maven/maven-3/3.8.1/binaries/apache-maven-3.8.1-bin.tar.gz
RUN tar -xzf apache-maven-3.8.1-bin.tar.gz
ENV MAVEN_HOME /redkale/apache-maven-3.8.1

ENV PATH $JAVA_HOME/bin:$MAVEN_HOME/bin:$PATH

COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

RUN cp /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=40000", "-Dbenchmarks.cache=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
