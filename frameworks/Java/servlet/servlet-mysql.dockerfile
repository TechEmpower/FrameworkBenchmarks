FROM tfb/maven:latest as maven
ADD ./ /servlet
WORKDIR /servlet
RUN mvn clean compile war:war -P mysql

FROM tfb/resin:latest
COPY --from=maven /servlet/target/servlet.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
