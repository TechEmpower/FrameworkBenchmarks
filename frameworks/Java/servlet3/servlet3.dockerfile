FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /servlet3
COPY src src
COPY pom.xml pom.xml
RUN mvn compile war:war -q

FROM tomcat:9.0.20-jre11-slim
WORKDIR /servlet3
RUN rm -rf ${CATALINA_HOME}/webapps/*
COPY --from=maven /servlet3/target/servlet3.war ${CATALINA_HOME}/webapps/ROOT.war
COPY server.xml ${CATALINA_HOME}/conf/server.xml

EXPOSE 8080

CMD bash ${CATALINA_HOME}/bin/catalina.sh run
