FROM techempower/maven:0.1 as maven

ADD ./ /curacao
WORKDIR /curacao
RUN mvn clean compile war:war

FROM techempower/resin:0.1

COPY --from=maven /curacao/target/curacao.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
