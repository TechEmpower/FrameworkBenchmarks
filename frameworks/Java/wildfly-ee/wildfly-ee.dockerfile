FROM maven:3-openjdk-17
WORKDIR /wildfly
EXPOSE 8080
COPY src src
COPY scripts scripts
COPY pom.xml pom.xml
RUN mvn clean package wildfly:package
CMD JAVA_OPTS="-Djava.net.preferIPv4Stack=true -XX:SoftMaxHeapSize=18g -Xmx24g -XX:+UseZGC" ./target/server/bin/standalone.sh
