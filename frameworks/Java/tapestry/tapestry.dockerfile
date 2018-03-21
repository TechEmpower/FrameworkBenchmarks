FROM tfb/maven-java8:latest as maven
ADD ./ /tapestry
WORKDIR /tapestry
RUN mvn clean compile war:war

FROM tfb/resin-java8:latest
COPY --from=maven /tapestry/target/tapestry.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
