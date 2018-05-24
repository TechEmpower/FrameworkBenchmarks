FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /servlet3
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM tomcat:9.0.7-jre10-slim
WORKDIR /servlet3
RUN rm -rf ${CATALINA_HOME}/webapps/*
COPY --from=maven /servlet3/target/servlet3.war ${CATALINA_HOME}/webapps/ROOT.war
COPY server.xml ${CATALINA_HOME}/conf/server.xml
CMD bash ${CATALINA_HOME}/bin/catalina.sh run
