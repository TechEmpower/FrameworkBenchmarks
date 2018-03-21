FROM tfb/maven:latest as maven
ADD ./ /curacao
WORKDIR /curacao
RUN mvn clean compile war:war

FROM tfb/resin:latest
COPY --from=maven /curacao/target/curacao.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
