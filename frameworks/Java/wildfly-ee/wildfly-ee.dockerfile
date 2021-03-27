FROM maven:3.6.3-openjdk-16
WORKDIR /wildfly
EXPOSE 8080
ENV MAVEN_OPTS="--add-exports=java.xml/com.sun.org.apache.xerces.internal.parsers=ALL-UNNAMED --add-exports=java.xml/com.sun.org.apache.xerces.internal.util=ALL-UNNAMED"
COPY src src
COPY scripts scripts
COPY pom.xml pom.xml
RUN mvn clean package -P bootable-jar
CMD java -Djava.net.preferIPv4Stack=true -Xmx24g -XX:+UseZGC -jar target/wildfly-ee-bootable.jar
