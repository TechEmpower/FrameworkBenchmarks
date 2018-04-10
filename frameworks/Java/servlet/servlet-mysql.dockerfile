FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /servlet
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q -P mysql

FROM techempower/resin:0.1
COPY --from=maven /servlet/target/servlet.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
