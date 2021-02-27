FROM maven:3.6.3-adoptopenjdk-15
WORKDIR /wildfly
EXPOSE 8080
COPY src src
COPY scripts scripts
COPY pom.xml pom.xml
RUN apt-get update
RUN apt-get install -yqq wget
RUN mvn clean package -P bootable-jar
CMD java -Djava.net.preferIPv4Stack=true -Xmx24g -XX:+UseZGC -jar target/wildfly-ee-bootable.jar
