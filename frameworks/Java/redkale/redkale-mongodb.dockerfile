FROM maven:3.8.4-openjdk-17-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
RUN rm conf/persistence.xml
RUN mv conf/persistence-mongodb.xml  conf/persistence.xml
COPY pom-mongodb.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=80000", "-DAPP_HOME=./", "-jar", "/redkale/target/redkale-benchmark-1.0.0.jar"]