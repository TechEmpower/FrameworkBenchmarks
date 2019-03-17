FROM maven:3.6.0-jdk-11 as maven

WORKDIR /greenlightning    
COPY pom.xml pom.xml
COPY src src

RUN mvn clean install -q

#COPY repo /usr/share/maven/ref/repository
#RUN mvn clean install -q -Dmaven.repo.local=/usr/share/maven/ref/repository

FROM azul/zulu-openjdk-alpine:11.0.1
WORKDIR /greenlightning
COPY --from=maven /greenlightning/target/greenlightning-test.jar app.jar

#records to our log all the known network settings on the host connection 
#CMD sysctl -a && java -server -Xmx26g -XX:+UseNUMA -jar app.jar

CMD java -server -Xmx26g -XX:+UseNUMA -jar app.jar
