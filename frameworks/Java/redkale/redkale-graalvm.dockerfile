FROM ubuntu:20.10
ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /redkale
RUN apt-get update -yqq
RUN apt-get install -yqq wget

RUN wget https://redkale.org/graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
RUN tar -xvzf graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
ENV JAVA_HOME /redkale/graalvm-ee-java16-21.2.0

RUN wget https://ftp.wayne.edu/apache/maven/maven-3/3.8.1/binaries/apache-maven-3.8.1-bin.tar.gz
RUN tar -xvzf apache-maven-3.8.1-bin.tar.gz
ENV MAVEN_HOME /redkale/apache-maven-3.8.1

ENV PATH $JAVA_HOME/bin:$MAVEN_HOME/bin:$PATH
RUN update-alternatives --install /usr/bin/java java $JAVA_HOME/bin/java
RUN update-alternatives --install /usr/bin/mvn mvn $MAVEN_HOME/bin/mvn

RUN pwd
RUN ls -lh

COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

COPY /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=40000", "-Dbenchmarks.cache=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
