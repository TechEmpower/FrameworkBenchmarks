FROM techempower/maven-java8:0.1 as maven

ADD ./ /tapestry
WORKDIR /tapestry
RUN mvn clean compile war:war

FROM techempower/resin-java8:0.1

COPY --from=maven /tapestry/target/tapestry.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
