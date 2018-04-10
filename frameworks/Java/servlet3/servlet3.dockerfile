FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /servlet3
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM techempower/tomcat:0.1
COPY --from=maven /servlet3/target/servlet3.war ${CATALINA_HOME}/webapps/ROOT.war
COPY server.xml ${CATALINA_HOME}/conf/server.xml
CMD bash ${CATALINA_HOME}/bin/catalina.sh run
