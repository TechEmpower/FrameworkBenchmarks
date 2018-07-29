FROM maven:3.5.3-jdk-10-slim as maven
ENV wfly=13.0.0.Final
ENV JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xms2g -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=50"
WORKDIR /wildfly
COPY src src
COPY pom.xml pom.xml
COPY wildfly-config.txt wildfly-config.txt
RUN apt-get update
RUN apt-get install -yqq wget
RUN wget -q -O- http://download.jboss.org/wildfly/$wfly/wildfly-$wfly.tar.gz | tar xz
RUN wget -q http://central.maven.org/maven2/mysql/mysql-connector-java/5.1.46/mysql-connector-java-5.1.46.jar -O mysql-connector-java.jar
RUN mvn clean package -q
RUN ./wildfly-$wfly/bin/jboss-cli.sh --file=wildfly-config.txt
CMD ./wildfly-$wfly/bin/standalone.sh -b 0.0.0.0