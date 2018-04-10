FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /wicket
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM techempower/resin:0.1
COPY --from=maven /wicket/target/hellowicket-1.0.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
