FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /tapestry
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM techempower/resin-java8:0.1
COPY --from=maven /tapestry/target/tapestry.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
