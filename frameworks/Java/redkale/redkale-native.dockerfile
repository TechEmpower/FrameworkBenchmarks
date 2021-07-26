FROM ubuntu:20.10
ARG DEBIAN_FRONTEND=noninteractive
ARG MAVEN_VERSION=3.8.1

WORKDIR /redkale
RUN apt-get update -yqq
RUN apt-get install -yqq wget

RUN wget --no-verbose https://redkale.org/graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
RUN tar -xzf graalvm-ee-java16-linux-amd64-21.2.0.tar.gz
ENV JAVA_HOME /redkale/graalvm-ee-java16-21.2.0

RUN wget --no-verbose https://ftp.wayne.edu/apache/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz
RUN tar -xzf apache-maven-${MAVEN_VERSION}-bin.tar.gz
ENV MAVEN_HOME /redkale/apache-maven-${MAVEN_VERSION}

ENV PATH $JAVA_HOME/bin:$MAVEN_HOME/bin:$PATH
RUN wget --no-verbose https://redkale.org/native-image-installable-svm-svmee-java16-linux-amd64-21.2.0.jar
RUN gu -L install ./native-image-installable-svm-svmee-java16-linux-amd64-21.2.0.jar

COPY src src
COPY conf conf
COPY pom.xml pom.xml
RUN mvn package -q

RUN cp /redkale/target/redkale-benchmark-1.0.0.jar redkale-benchmark.jar
RUN native-image -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar redkale-benchmark.jar

EXPOSE 8080

CMD ./redkale-benchmark 
