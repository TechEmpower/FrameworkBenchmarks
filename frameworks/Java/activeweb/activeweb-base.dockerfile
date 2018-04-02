FROM techempower/maven:0.1 as maven
ADD ./ /activeweb
WORKDIR /activeweb
RUN mvn clean package -DskipTests

FROM techempower/resin:0.1
COPY --from=maven /activeweb/target/activeweb.war ${RESIN_HOME}/webapps/ROOT.war
