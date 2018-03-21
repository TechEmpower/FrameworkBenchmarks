FROM tfb/maven:latest as maven
ADD ./ /wicket
WORKDIR /wicket
RUN mvn clean compile war:war

FROM tfb/resin:latest
COPY --from=maven /wicket/target/hellowicket-1.0.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
