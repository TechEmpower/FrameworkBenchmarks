FROM adoptopenjdk/maven-openjdk13:latest as maven
ENV wfly=18.0.0.Final
ENV JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xms2g -Xmx2g"
WORKDIR /wildfly
COPY src src
COPY pom.xml pom.xml
COPY wildfly-config.txt wildfly-config.txt
RUN apt-get update
RUN apt-get install -yqq wget
RUN wget -q -O- http://download.jboss.org/wildfly/$wfly/wildfly-$wfly.tar.gz | tar xz
RUN wget -q http://central.maven.org/maven2/mysql/mysql-connector-java/8.0.16/mysql-connector-java-8.0.16.jar -O mysql-connector-java.jar
RUN mvn clean package -q
RUN ./wildfly-$wfly/bin/jboss-cli.sh --file=wildfly-config.txt
CMD ./wildfly-$wfly/bin/standalone.sh -b 0.0.0.0
