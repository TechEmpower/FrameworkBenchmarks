FROM tfb/maven:latest as maven
ADD ./ /curacao
WORKDIR /curacao
RUN mvn clean compile war:war

FROM tfb/resin:latest
COPY --from=maven /curacao/target/curacao.war /var/resin/webapps/ROOT.war
CMD resinctl console
