FROM tfb/maven-java8:latest as maven
ADD ./ /spark
WORKDIR /spark
RUN mvn clean package

FROM tfb/resin-java8
COPY --from=maven /spark/target/spark.war /var/resin/webapps/ROOT.war
CMD resinctl console
