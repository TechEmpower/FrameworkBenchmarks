FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /activeweb
COPY pom.xml pom.xml
COPY scripts scripts
COPY src src
RUN mvn package -DskipTests -q

FROM techempower/resin:0.1
COPY --from=maven /activeweb/target/activeweb.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
