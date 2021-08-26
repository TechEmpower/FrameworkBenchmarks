FROM openjdk:17-jdk-slim
ARG DEBIAN_FRONTEND=noninteractive
ARG MAVEN_VERSION=3.8.1

WORKDIR /redkale
RUN apt-get update -yqq
RUN apt-get install -yqq wget

RUN wget --no-verbose https://ftp.wayne.edu/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz
RUN tar -xzf apache-maven-${MAVEN_VERSION}-bin.tar.gz
ENV MAVEN_HOME /redkale/apache-maven-${MAVEN_VERSION}
ENV PATH $MAVEN_HOME/bin:$PATH

COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

RUN cp /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=80000", "-Dbenchmarks.cache=true", "-DAPP_HOME=./", "-jar", "redkale-benchmark.jar"]
