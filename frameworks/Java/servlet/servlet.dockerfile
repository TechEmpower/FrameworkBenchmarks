FROM tfb/maven:latest as maven
ADD ./ /servlet
WORKDIR /servlet
RUN mvn clean compile war:war

FROM tfb/resin:latest
COPY --from=maven /servlet/target/servlet.war /var/resin/webapps/ROOT.war
CMD resinctl console
