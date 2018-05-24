FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /wildfly
COPY server-resources server-resources
COPY src src
COPY pom.xml pom.xml
RUN mvn initialize package -q -P benchmark
ENV JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xms2g -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=50"
CMD target/wildfly-12.0.0.Final/bin/standalone.sh -b 0.0.0.0
