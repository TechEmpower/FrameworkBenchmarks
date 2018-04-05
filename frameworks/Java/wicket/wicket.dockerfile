FROM techempower/maven:0.1 as maven
ADD ./ /wicket
WORKDIR /wicket
RUN mvn clean compile war:war

FROM techempower/resin:0.1
COPY --from=maven /wicket/target/hellowicket-1.0.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
