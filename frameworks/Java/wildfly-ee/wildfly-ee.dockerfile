FROM adoptopenjdk/maven-openjdk13:latest as maven
ENV wfly=18.0.1.Final
ENV connectorj=8.0.19
ENV JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xmx24g -XX:+UnlockExperimentalVMOptions -XX:+UseZGC"
WORKDIR /wildfly
COPY src src
COPY pom.xml pom.xml
COPY wildfly-config.txt wildfly-config.txt
RUN apt-get update
RUN apt-get install -yqq wget
RUN wget -q -O- https://download.jboss.org/wildfly/$wfly/wildfly-$wfly.tar.gz | tar xz
RUN wget -q https://repo1.maven.org/maven2/mysql/mysql-connector-java/$connectorj/mysql-connector-java-$connectorj.jar -O mysql-connector-java.jar
RUN mvn clean package -q
RUN ./wildfly-$wfly/bin/jboss-cli.sh --file=wildfly-config.txt
CMD ./wildfly-$wfly/bin/standalone.sh -b 0.0.0.0
