FROM maven:3.8.6-openjdk-18-slim as maven
WORKDIR /redkale
COPY src src
COPY conf conf
RUN rm conf/source.properties
RUN mv conf/source-mysql.properties  conf/source.properties
COPY pom.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:AutoBoxCacheMax=80000", "-DAPP_HOME=./", "-jar", "/redkale/target/redkale-benchmark-1.0.0.jar"]