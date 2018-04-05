FROM techempower/maven:0.1 as maven

ADD ./ /servlet3
WORKDIR /servlet3
RUN mvn clean compile war:war

FROM techempower/tomcat:0.1

COPY --from=maven /servlet3/target/servlet3.war ${CATALINA_HOME}/webapps/ROOT.war
COPY --from=maven /servlet3/server.xml ${CATALINA_HOME}/conf/server.xml
CMD bash ${CATALINA_HOME}/bin/catalina.sh run
