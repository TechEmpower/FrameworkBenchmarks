FROM tfb/maven:latest as maven
ADD ./ /servlet3
WORKDIR /servlet3
RUN mvn clean compile war:war -P sync

FROM tfb/tomcat:latest
COPY --from=maven /servlet3/target/servlet3.war ${CATALINA_HOME}/webapps/ROOT.war
COPY --from=maven /servlet3/server.xml ${CATALINA_HOME}/conf/server.xml
CMD bash ${CATALINA_HOME}/bin/catalina.sh run
