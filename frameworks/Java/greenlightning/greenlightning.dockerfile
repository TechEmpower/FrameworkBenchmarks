FROM maven:3.6.1-jdk-11 as maven

WORKDIR /greenlightning
COPY pom.xml pom.xml
COPY src src

RUN mvn clean install -q

#COPY repo /usr/share/maven/ref/repository
#RUN mvn clean install -q -Dmaven.repo.local=/usr/share/maven/ref/repository

FROM azul/zulu-openjdk-alpine:11.0.3
WORKDIR /greenlightning
COPY --from=maven /greenlightning/target/greenlightning-test.jar app.jar

EXPOSE 8080

CMD java -server -Xmx29g -XX:AutoBoxCacheMax=1000000 -XX:NewSize=64m -XX:+UseNUMA -jar app.jar
