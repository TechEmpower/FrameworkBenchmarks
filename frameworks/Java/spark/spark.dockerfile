FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /spark
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM techempower/resin-java8:0.1
COPY --from=maven /spark/target/spark.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
