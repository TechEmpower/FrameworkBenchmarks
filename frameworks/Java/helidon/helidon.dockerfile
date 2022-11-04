FROM openjdk:19-jdk-slim as maven

RUN apt-get update \
  && apt-get install -y curl procps \
  && rm -rf /var/lib/apt/lists/*
ARG MAVEN_VERSION=3.8.6
ARG USER_HOME_DIR="/root"
ARG SHA=f790857f3b1f90ae8d16281f902c689e4f136ebe584aba45e4b1fa66c80cba826d3e0e52fdd04ed44b4c66f6d3fe3584a057c26dfcac544a60b301e6d0f91c26
ARG BASE_URL=https://apache.osuosl.org/maven/maven-3/${MAVEN_VERSION}/binaries
RUN mkdir -p /usr/share/maven /usr/share/maven/ref \
  && curl -fsSL -o /tmp/apache-maven.tar.gz ${BASE_URL}/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
  && echo "${SHA}  /tmp/apache-maven.tar.gz" | sha512sum -c - \
  && tar -xzf /tmp/apache-maven.tar.gz -C /usr/share/maven --strip-components=1 \
  && rm -f /tmp/apache-maven.tar.gz \
  && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn
ENV MAVEN_HOME /usr/share/maven
ENV MAVEN_CONFIG "$USER_HOME_DIR/.m2"

WORKDIR /helidon
COPY reactive/src src
COPY reactive/pom.xml pom.xml
RUN mvn package -q

FROM openjdk:19-jdk-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark-reactive.jar app.jar

EXPOSE 8080

CMD java -server \
    -Dio.netty.buffer.checkBounds=false \
    -Dio.netty.buffer.checkAccessible=false \
    -jar app.jar
